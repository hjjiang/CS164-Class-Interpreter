"""Exposes a set of layout and utility functions to be invoked from 164. These
These functions have access to the global browser instance.
"""
import json
from urllib import urlencode, urlopen
from urlparse import urlparse, urlunparse, parse_qsl
from warnings import warn
import sys

from PyQt4.QtNetwork import QNetworkReply
from browser import Browser, Renderer

import grammar_parser
import parser_generator
import interpreter

tmlGrm = './tml.grm'
cs164Grm = 'cs164c.grm'
cs164Lib = 'library.164'

def initialize():
    global _browser
    # Initialize reusable parts of the browser infrastructure.
    tmlParser = parser_generator.makeParser(grammar_parser.parseFile(tmlGrm))
    print >> sys.stderr, 'TML parser loaded.'
    cs164Parser = parser_generator.makeParser(grammar_parser.parseFile(cs164Grm))
    print >> sys.stderr, 'CS164 parser loaded.'
    # TODO: Currently, the interpreter "object" is the module itself. Later,
    # expose an API for constructing interpreters.
    renderer = Renderer(interpreter)
    print >> sys.stderr, 'Renderer initialized.'
    _browser = Browser(tmlParser, cs164Parser, interpreter, renderer)
    interpreter.ExecGlobal(cs164Parser.parse(open(cs164Lib).read()))
    print >> sys.stderr, 'Browser initialized.'
    print >> sys.stderr, 'Loading the layout engine modules...'
    # Load the layout engine, which is implemented in 164.
    dependencies = ['object', 'node', 'window', 'box',
                    'hbox', 'vbox', 'link', 'word', 'img', 'script',
                    'bquery', 'rx',
                    #TODO: Uncomment for PA8 'pbar', 'ibox'
                    #TODO: Uncomment for PA9 'rx', 
                    'layout', 'browser']
    for dependency in dependencies:
        print >> sys.stderr, '\tloading ' + dependency
        ast = cs164Parser.parse(open('./browser/{0}.164'.format(dependency)).read())
        interpreter.ExecGlobal(ast)
    print >> sys.stderr, 'Done.'

def clear():
    _browser.clear(_browser.window)

def drawBox(x, y, width, height, properties, widget=None):
    return _browser.renderer.drawBox(x, y, width, height, properties,
                                     _browser.window.canvas, widget)

def drawWord(word, x, y, properties, widget=None):
    return _browser.renderer.drawWord(word, x, y, properties,
                                      _browser.window.canvas, widget)

def drawInput(x, y, width, height, text, widget=None):
  return _browser.renderer.drawInput(x, y, width, height, text,
                                     _browser.window.canvas, widget)
def drawPBar(x, y, width, height, value, widget=None):
  return _browser.renderer.drawPBar(x, y, width, height, value,
                                     _browser.window.canvas, widget)
def drawImg(x, y, width, height, rawdata, widget=None):
  return _browser.renderer.drawImg(x, y, width, height, rawdata,
                                     _browser.window.canvas, widget)
def getWordDimensions(word, properties):
    return _browser.renderer.getWordDimensions(word, properties)

def setDOMWindowSize(width, height):
    """Sets the size of the window in the DOM. This is not the application
    window.
    """
    _browser.window.canvas.setMinimumSize(width, height)
    _browser.window.canvas.resize(width, height)

def parseTML(tml):
    """Returns the raw DOM object constructed by the TML parser."""
    return _browser.tmlParser.parse(tml)

def evaluateScript(url):
    """Evaluates the script at the given URL."""
    connection = urlopen(url)
    code = connection.read()
    connection.close()
    _browser.execScript(code)

def load(url):
    print >> sys.stderr, 'Loading', url
    _browser.load(url)

def create164Callback(node, code):
    """Run the specified string of 164 code when an event is fired on the
    target node. The 'code' argument may be a 164 function instead of a string
    of code to allow the 164 programmer to pass in lambdas as callbacks.
    """
    assert (isinstance(code, basestring) or
            isinstance(code, interpreter.FunVal)), \
            'callback must be a string or closure'

    def callback(ev = None, relayout = False):
        if isinstance(code, basestring):
            _browser.execScript(code, {'self': node, 'event': ev})
        else:
            _browser.interpreter.ExecFun(code, [ev])
        if (relayout):
            _browser.relayout()
    return callback

def addEventListener(node, event, code, context=None):
    """Subscribes to an event on the given node, executing the specified code
    when such an event is triggered. A "context" node may be specified to
    indicate the node that is the "self" object for the callback.
    """
    callback = create164Callback(node if context is None else context, code)

    # For each event type, we need to define custom handlers (Qt slots) that
    # convert Qt events into 164 event objects.
    if event == 'click':
        def clickHandler(ev):
            event = {'x': ev.x(), 'y': ev.y(),
                     'screenX': ev.globalX(), 'screenY': ev.globalY()}
            callback(event, True)
        node['__qt'].clicked.connect(clickHandler)
    elif event == 'textChanged':
        def changeHandler(text):
            event = {'text': text}
            callback(event, False)
        node['__qt'].textChanged.connect(changeHandler)
    elif event == 'focusChanged':
        def focusHandler(ev):
            event = {'gotFocus': ev.gotFocus(), 'lostFocus': ev.lostFocus()}
            callback(event, False)
        node['__qt'].focused.connect(focusHandler)
    elif event == 'editingFinished':
        def editHandler():
            callback({}, False)
        node['__qt'].editingFinished.connect(editHandler)
    else:
        raise TypeError("Unknown event " + str(event))

def removeClickEvent(node):
    node['__qt'].clicked.disconnect()
        
def addAnchorTarget(node, url):
    node['__qt'].clicked.connect(lambda _: load(url))

def addTimerCallback(window, ms, code, repeat=False):
    callback = create164Callback(window, code)
    timer = _browser.createTimer()
    timer.setInterval(ms)
    timer.setSingleShot(not repeat)
    timer.timeout.connect(callback)
    return timer.timerId()

def sendHttp(window, uri, code):
    reply = _browser.createNetworkReply(uri)
    callback = create164Callback(window, code)

    def responseHandler():
        if reply.error() == QNetworkReply.NoError:
            data = reply.readAll().data()
            try:
                data = json.loads(data)
            except ValueError:
                # The data is not JSON.
                pass
            else:
                data = jsonTo164Object(data)
            response = {'status': 'ok', 'response': data}
        else:
            response = {'status': 'error', 'response': reply.errorString()}
        callback(response, False)

    reply.finished.connect(responseHandler)

    # Create an object that 164 can handle.
    return {'__qt': reply}

def setFocus(node):
    """Set the focus onto the Qt widget underneath "node"."""
    _browser.setFocus(node)

#TODO Complete me
def jsonTo164Object(json):
    """Convert the JSON string "json" into a 164 object."""
    # Essentially need to convert the python list into a 164 list
    # 164 lists are dictionaries indexed by ints from 0 and onward
    # so we just need to iterate through the python list and turn it
    # a dictionary with that sort of key/value and that's our 164 list
    retList = {}
    counter = 0

    # this if only works for youtube....
    if (type(json) == dict):
        for element in json.values():
            if (type(element) == dict):
                for e in element.values():
                    if (type(e) == list):
                        for e2 in e:
                            retList[counter] = e2
                            counter = counter + 1 

    else:
        for element in json:
            retList[counter] = element
            counter = counter + 1
    #print retList
    return retList
    # pass
    
def doQuote(str):
    """Add quotes around "str"."""
    return "\"" + str + "\""
