"""Functions for TML layout that are used in the grammar to construct DOM-like
node objects used in the 164 layout engine.
""" 

def createNode(name, attributes=None, children=None):
    """Creates a DOM-like node object, using the 164 representation so that
    the node can be processed by the 164 layout engine.
    """
    node = dict(attributes)
    node['name'] = name
    # Represent the list of child nodes as a dict with numeric keys.
    node['children'] = dict(enumerate(children)) if children else {}
    return node

def createWordNodes(text):
    """Returns a Python list of DOM-like nodes, one for each word in the given
    text.
    """
    return [createNode('Word', {'word': word + ' '}) for word in text.split()]
