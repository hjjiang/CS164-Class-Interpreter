import re
from collections import defaultdict
from warnings import warn
from PyQt4.QtCore import pyqtSignal, Qt
from PyQt4.QtGui import *

class QDomNode(QWidget):

    clicked = pyqtSignal(QMouseEvent)
    focused = pyqtSignal(QFocusEvent)
    
    def __init__(self, parent=None, flags=0):
        # SIP code expects the flags to be of type Qt.WindowFlags.
        if not isinstance(flags, Qt.WindowFlags):
            flags = Qt.WindowFlags(int(flags))
        QWidget.__init__(self, parent, flags)
        
    def mouseReleaseEvent(self, ev):
        # A click occurred if the mouse button was released inside the box.
        if 0 <= ev.x() < self.width() and 0 <= ev.y() < self.height():
            self.clicked.emit(ev)
            
    def focusInEvent(self, event):
        self.focused.emit(event)
    
    def focusOutEvent(self, event):
        self.focused.emit(event)

class QDomBasicElement(QDomNode):
    """A Qt widget that forms the basis for DOM nodes. This differs from the
    DOM-specified Node interface in that instances of this class can have some
    general stylistic properties.
    """

    def __init__(self, parent=None, flags=0):
        QDomNode.__init__(self, parent, flags)
        
        self.color = 'black'
        self.opacity = '1.0'

    def setProperties(self, properties):
        if 'color' in properties:
            self.color = properties['color']
        if 'opacity' in properties:
            self.opacity = properties['opacity']

    @property
    def color(self):
        return self._color

    @color.setter
    def color(self, color):
        newColor = QColor(color)
        if not newColor.isValid():
            warn('"{0}" is not a valid color'.format(color))
        else:
            self._color = newColor

    @property
    def opacity(self):
        return self._opacity

    @opacity.setter
    def opacity(self, opacity):
        try:
            self._opacity = float(opacity)
        except ValueError:
            warn('"{0}" is not a valid opacity level'.format(opacity))


class QDomElement(QDomBasicElement):
    """A Qt widget that simulates a traditional CSS box. Only the stylistic
    properties of CSS, such as color and opacity, are supported. Properties
    related to layout, such as width and margin, are omitted because it is the
    164 layout algorithm's job to compute the positions and sizes of boxes.
    
    Instances of this class can be composed with other Qt widgets, such as
    buttons and text fields. This allows for a richer browser experience.
    """

    def __init__(self, parent=None, flags=0):
        QDomBasicElement.__init__(self, parent, flags)

        # Define a set of decorative CSS properties that are supported.
        self.backgroundColor = 'transparent'

    def setProperties(self, properties):
        QDomBasicElement.setProperties(self, properties)
        if 'backgroundColor' in properties:
            self.backgroundColor = properties['backgroundColor']

    @property
    def backgroundColor(self):
        return self._backgroundColor

    @backgroundColor.setter
    def backgroundColor(self, color):
        newBackgroundColor = QColor(color)
        if not newBackgroundColor.isValid():
            warn('"{0}" is not a valid background color'.format(color))
        else:
            self._backgroundColor = newBackgroundColor

    def paintEvent(self, ev):
        painter = QPainter(self)
        painter.setOpacity(self.opacity)

        # Paint the background of the box.
        painter.fillRect(0, 0, self.width(), self.height(),
                         self.backgroundColor)

        # TODO: Add support for border properties.
        painter.setPen(QColor('black'))
        painter.drawRect(0, 0, self.width() - 1, self.height() - 1)


class QDomText(QDomBasicElement):
    """A Qt widget that simulates a text node. The design of this class differs
    from a traditional browser in that text nodes are directly styled, whereas
    a conventional browser decorates text using the styles of parent elements.
    """

    FONT_STYLES = defaultdict(lambda: QFont.StyleNormal,
                              {'italic': QFont.StyleItalic,
                               'oblique': QFont.StyleOblique})
    FONT_SIZE_REGEX = re.compile(r'^(\d*(?:\.\d+)?)(pt|px)?$')

    def __init__(self, text, parent=None, flags=0):
        QDomBasicElement.__init__(self, parent, flags)
        self.text = text
        self.font = QFont()

        # Initialize the relevant decorative CSS properties.
        self.fontFamily = 'Helvetica'
        self.fontStyle = 'normal'
        self.fontVariant = 'normal'
        self.fontSize = '12px'
        self.fontWeight = 'normal'
        self.textDecoration = 'none'
        self.textTransform = 'none'

    def setProperties(self, properties):
        QDomBasicElement.setProperties(self, properties)
        if 'fontFamily' in properties:
            self.fontFamily = properties['fontFamily']
        if 'fontStyle' in properties:
            self.fontStyle = properties['fontStyle']
        if 'fontVariant' in properties:
            self.fontVariant = properties['fontVariant']
        if 'fontSize' in properties:
            self.fontSize = properties['fontSize']
        if 'fontWeight' in properties:
            self.fontWeight = properties['fontWeight']
        if 'textDecoration' in properties:
            self.textDecoration = properties['textDecoration']
        if 'textTransform' in properties:
            self.textTransform = properties['textTransform']

    @property
    def fontFamily(self):
        return self._fontFamily

    @fontFamily.setter
    def fontFamily(self, family):
        self.font.setFamily(str(family))
        self._fontFamily = family

    @property
    def fontStyle(self):
        return self._fontStyle

    @fontStyle.setter
    def fontStyle(self, style):
        self.font.setStyle(QDomText.FONT_STYLES[style])
        self._fontStyle = style

    @property
    def fontVariant(self):
        return self._fontVariant

    @fontVariant.setter
    def fontVariant(self, variant):
        capitalization = (QFont.SmallCaps if variant == 'small-caps'
                          else QFont.MixedCase)
        self.font.setCapitalization(capitalization)
        self._fontVariant = variant

    @property
    def fontSize(self):
        return self._fontSize

    @fontSize.setter
    def fontSize(self, size):
        size = str(size)
        match = QDomText.FONT_SIZE_REGEX.match(size)
        if not match:
            warn('"{0}" is not a valid font size'.format(size))
        else:
            size, units = match.groups()
            if units == 'pt':
                self.font.setPointSizeF(float(size))
            else:
                self.font.setPixelSize(int(size))
            self._fontSize = size

    @property
    def textDecoration(self):
        return self._textDecoration

    @textDecoration.setter
    def textDecoration(self, decoration):
        self.font.setUnderline(decoration == 'underline')
        self.font.setOverline(decoration == 'overline')
        self.font.setStrikeOut(decoration == 'line-through')
        self._textDecoration = decoration

    def getTransformedText(self):
        text = unicode(self.text)
        if self.textTransform == 'capitalize':
            text = self.text.capitalize()
        elif self.textTransform == 'uppercase':
            text = self.text.upper()
        elif self.textTransform == 'lowercase':
            text = self.text.lower()
        return text

    def getDimensions(self):
        metrics = QFontMetrics(self.font)
        return metrics.width(self.getTransformedText()), metrics.height()

    def paintEvent(self, ev):
        QDomBasicElement.paintEvent(self, ev)
        painter = QPainter(self)
        painter.setPen(self.color)
        painter.setFont(self.font)
        text = self.getTransformedText()
        painter.drawText(0, QFontMetrics(self.font).ascent(), text)
        
class QMyLineEdit(QLineEdit):

    focused = pyqtSignal(QFocusEvent)

    def __init__(self, parent=None):
        QLineEdit.__init__(self, parent)

    def focusInEvent(self, event):
        QLineEdit.focusInEvent(self, event)
        self.focused.emit(event)
    
    def focusOutEvent(self, event):
        QLineEdit.focusOutEvent(self, event)
        self.focused.emit(event)
