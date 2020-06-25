# pylint: disable=C0111
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
config = config  # type: ConfigAPI # noqa: F821 pylint: disable=E0602,C0103
c = c  # type: ConfigContainer # noqa: F821 pylint: disable=E0602,C0103

config.load_autoconfig()

c.content.pdfjs = True

c.url.default_page = 'https://raw.githubusercontent.com/qutebrowser/qutebrowser/master/doc/img/cheatsheet-big.png'
c.url.searchengines = {'DEFAULT': 'https://www.google.com/search?hl=en&q={}'}
c.url.start_pages = 'https://raw.githubusercontent.com/qutebrowser/qutebrowser/master/doc/img/cheatsheet-big.png'

# c.editor.command = ["gnome-terminal", "--wait", "--", "vim", "+call cursor({line}, {column})", "--", "{file}"]

c.zoom.default = '140%'
c.fonts.hints = 'bold 14pt default_family'
c.colors.hints.bg = 'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 rgba(255, 247, 133, 1.0), stop:1 rgba(255, 197, 66, 1.0))'
c.window.hide_decoration = True

config.bind('h', 'tab-prev')
config.bind('l', 'tab-next')

config.bind('tp', 'tab-pin')

config.bind('<Ctrl-i>', 'forward')
config.bind('<Ctrl-o>', 'back')

config.bind('<Ctrl-n>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-p>', 'completion-item-focus --history prev', mode='command')

config.bind('<Ctrl-y>', 'command-accept', mode='command')

c.auto_save.session = True
