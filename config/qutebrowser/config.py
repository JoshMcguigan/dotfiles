# pylint: disable=C0111
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
config = config  # type: ConfigAPI # noqa: F821 pylint: disable=E0602,C0103
c = c  # type: ConfigContainer # noqa: F821 pylint: disable=E0602,C0103

config.load_autoconfig()

c.content.pdfjs = True

c.url.default_page = 'https://www.github.com'
c.url.start_pages = c.url.default_page
c.url.searchengines = {'DEFAULT': 'https://www.google.com/search?hl=en&q={}'}

c.editor.command = ["gnome-terminal", "--wait", "--", "vim", "+call cursor({line}, {column})", "--", "{file}"]

c.window.hide_decoration = True

config.bind('h', 'tab-prev')
config.bind('l', 'tab-next')
config.bind('gl', 'tab-move -')
config.bind('gr', 'tab-move +')

config.bind('tp', 'tab-pin')

config.bind('<Ctrl-i>', 'forward')
config.bind('<Ctrl-o>', 'back')

config.bind('<Ctrl-n>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-p>', 'completion-item-focus --history prev', mode='command')

config.bind('<Ctrl-y>', 'command-accept', mode='command')

c.auto_save.session = True

config.source('nord.py')
