#!/usr/bin/python
import sys
from urllib import urlopen
from optparse import OptionParser

from PyQt4.QtCore import Qt, QTimer, QUrl
from PyQt4.QtGui import *
from PyQt4.QtNetwork import QNetworkAccessManager, QNetworkRequest
from qtdom import *

class Browser(QApplication):
    """The browser application that comprises a parser, interpreter, renderer,
    and other core components of a browser.
    """

    APPLICATION_NAME = 'Internet Exploder 164'

    def __init__(self, tmlParser, cs164Parser, interpreter, renderer):
        QApplication.__init__(self, [Browser.APPLICATION_NAME])
        self.tmlParser = tmlParser
        self.cs164Parser = cs164Parser
        self.interpreter = interpreter
        self.renderer = renderer
        self.network = QNetworkAccessManager(self)
		
        self.window = Window()
        self.window.resize(610, 410)
        self.window.center()
        self.window.show()

        self.timers = {}
        self.networkReplies = []
        
        self.focused = None

    def load(self, url):
        self.reset(self.window)
        connection = urlopen(url)
        try:
            dom = self.tmlParser.parse(connection.read())
            self.renderer.render(dom, self.window.canvas)
        finally:
            connection.close()

    def createTimer(self):
        timer = QTimer()
        timer.start()
        self.timers[timer.timerId()] = timer
        print >> sys.stderr, 'Created new timer with ID', timer.timerId()
        return timer

    def createNetworkReply(self, uri):
        url = QUrl(uri, QUrl.TolerantMode)
        request = QNetworkRequest(url)
        reply = self.network.get(request)
        self.networkReplies.append(reply)
        return reply
            
    def setFocus(self, node):
        node['__qt'].setFocus()

    def clear(self, window):
        self.renderer.clear(window.canvas)

    def reset(self, window):
        self.clear(window)

        # Clear all of the timers.
        for timerId, timer in self.timers.items():
            timer.stop()
            timer.timeout.disconnect()
        self.timers = {}

        # Close up all HTTP connections.
        for reply in self.networkReplies:
            reply.finished.disconnect()
            reply.abort()
        self.networkReplies = []

    def relayout(self):
        self.execFun('relayout')

    def execScript(self, code, env = {}):
        ast = self.cs164Parser.parse(code)
        self.interpreter.ExecGlobal(ast, env.copy())
        
    def execFun(self, funName):
        self.interpreter.ExecFun(self.interpreter.globEnv[funName], [])
        
    @staticmethod
    def run():
        """Launches the browser and returns its exit status code when
        closed. This method may never return; for example, if the OS
        immediately shuts down, further code may never run.
        """
        # TODO: Perhaps run this in another thread.
        return QApplication.exec_()

class Renderer(object):

    def __init__(self, interpreter):
        self.interpreter = interpreter
    
    def render(self, dom, canvas):
        self.clear(canvas)
        # Invoke the 164 layout engine, which calls back into the renderer.
        self.interpreter.ExecFun(self.interpreter.globEnv['layoutRawDOM'], [dom])

    def drawBox(self, x, y, width, height, properties, canvas, widget=None):
        """Draws a box at the specified position with the given dimensions and
        style properties.
        """
        if widget is None:
            box = QDomElement(canvas)
        else:
            box = widget
            box.setParent(canvas)
        box.setProperties(properties)
        box.setFixedSize(width, height)
        box.move(x, y)
        box.show()
        return box

    def drawInput(self, x, y, width, height, text, canvas, widget=None):
        """Draws an input box at the specified position with the given dimensions
        and style properties.
        """
        if widget is None:
            input = QMyLineEdit(canvas)
        else:
            input = widget
            input.setParent(canvas)
        input.setText(text)
        input.setFixedSize(width, height)
        input.move(x, y)
        input.show()
        return input

    def drawPBar(self, x, y, width, height, value, canvas, widget=None):
        """Draws an input box at the specified position with the given dimensions
        and style properties.
        """
        if widget is None:
            pbar = QProgressBar(canvas)
        else:
            pbar = widget
            pbar.setParent(canvas)
        pbar.setRange(0, 100)
        pbar.setValue(int(value))
        pbar.setFixedSize(width, height)
        pbar.move(x, y)
        pbar.show()
        return pbar
    
    def drawImg(self, x, y, width, height, rawdata, canvas, widget=None):
        """Draws an input box at the specified position with the given dimensions
        and style properties.
        """
        if widget is None:
            px = QPixmap()
            px.loadFromData(rawdata)
            lbl = QLabel(canvas)
            lbl.setPixmap(px)
        else:
            lbl = widget
            lbl.setParent(canvas)
        lbl.setFixedSize(width, height)
        lbl.setScaledContents(True)
        lbl.move(x, y)
        lbl.show()
        return lbl

    def drawWord(self, word, x, y, properties, canvas, widget=None):
        """Draws the specified word at the given position on the canvas. The
        word is styled according to the given properties.
        """
        if widget is None:
            textNode = QDomText(word, canvas)
        else:
            textNode = widget
            textNode.text = word
            textNode.setParent(canvas)
        textNode.setProperties(properties)
        # TODO: Perhaps there is a better place to resize the text box.
        width, height = textNode.getDimensions()
        textNode.setFixedSize(width, height)
        textNode.move(x, y)
        textNode.show()
        return textNode

    def getWordDimensions(self, word, properties):
        """Returns a tuple containing the (width, height) of the rendered word
        with the given style properties.
        """
        textNode = QDomText(word)
        textNode.setProperties(properties)
        return textNode.getDimensions()

    def clear(self, canvas):
        """Clears the canvas of its contents so that it is blank."""
        # Copy the list of children so that it is not "live".
        childWidgets = [c for c in canvas.children() if c.isWidgetType()]
        for child in childWidgets:
            child.setParent(None)
        canvas.setMinimumSize(600, 400)

class Window(QMainWindow):

    def __init__(self, parent=None, flags=0):
        if not isinstance(flags, Qt.WindowFlags):
            flags = Qt.WindowFlags(int(flags))
        QMainWindow.__init__(self, parent, flags)

        # Set up the children components to display.
        self.scrollArea = QScrollArea(self)
        self.canvas = QDomElement(self.scrollArea)

        self.scrollArea.setWidget(self.canvas)
        self.scrollArea.setAlignment(Qt.AlignHCenter)
        self.setCentralWidget(self.scrollArea)

    def show(self):
        QMainWindow.show(self)
        self.scrollArea.show()
        self.canvas.show()

    def center(self):
        center = QDesktopWidget().availableGeometry().center()
        geometry = self.frameGeometry()
        geometry.moveCenter(center)
        self.move(geometry.topLeft())


if __name__ == '__main__':
    parser = OptionParser(usage='%prog [file...]')
    options, arguments = parser.parse_args()

    if arguments:
        inputFile = arguments[0]
    else:
        inputFile = './tests/bquery/demo.tml'

    import runtime
    runtime.initialize()
    runtime.load(inputFile)
    Browser.run()
