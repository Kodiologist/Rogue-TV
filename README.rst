Rogue TV is a nonviolent roguelike where you play a lucky contestant on a roguelike-themed game show. It draws inspiration from `Smash TV`_, `Weekend Warrior`_, MXC_, `Legends of the Hidden Temple`_, `Mazeworld Catacombs`_, `Scarab of RA`_, NetHack_, and, of course, Rogue_. It's written in Hy_ with Python 2.7.

Currently, the game is playable, but a lot of features remain to be implemented.

Dependencies include Hy_ (0.10.1, and no later), Kodhy_, and Heidegger_. Currently, curses is also a requirement. (All dependencies not already included with Python are in pure Python or Hy.) You can download the game and its dependencies as a self-contained bundle at http://arfer.net/downloads/roguetv-posix.tar.gz . The bundle has been tested on Linux, Mac OS X, and Windows (via Cygwin_), and should also work in other Unix-like environments. I will probably provide a self-contained Windows port eventually.

Start the game with ``sh run.sh`` (if you're using the bundle) or ``hy run.hy`` (if you're using the source code). Use the ``--help`` argument to see command-line options. Currently, the game requires a terminal emulator that supports 256 colors and Unicode, such as GNOME Terminal, Konsole, LXTerminal, or Terminal.app. It works fine over SSH or in screen or tmux.

Please `send me`__ bug reports and patches. Feature suggestions are also welcome, but I've got a lot planned already.

.. __: http://arfer.net/elsewhere

How to play
============================================================

Press the ``?`` key to see controls.

Detailed descriptions of every item, creature, and kind of terrain are available in-game. To see them, enter look mode (``;``) or the inventory list (``i``) and press the appropriate key.

In the status bar, the number in parenthesis next to the time remaining is the amount of time your last action took, in seconds. If there is no number, your last action took no time.

The object of the game is to maximize your winnings. Your winnings are the sum of the monetary values of the items you're carrying. More broadly, the goal is to maximize your *average* winnings across many games of Rogue TV. If you take an up elevator, or you win the game (by taking the final down elevator with the Amulet of Yendor), you get to keep all your winnings. But if you run out of time or resign, prizes are taken away from you until you have half your original winnings or less. So, it's sometimes wiser to take an up elevator than to press your luck in pursuit of the Amulet.

.. _Hy: http://hylang.org
.. _Kodhy: https://github.com/Kodiologist/Kodhy
.. _Heidegger: https://github.com/Kodiologist/Heidegger
.. _Cygwin: https://cygwin.com

.. _Smash TV: http://en.wikipedia.org/wiki/Smash_TV
.. _Weekend Warrior: http://www.pangeasoft.net/weekendwarrior.html
.. _Legends of the Hidden Temple: http://en.wikipedia.org/wiki/Legends_of_the_Hidden_Temple
.. _MXC: http://en.wikipedia.org/wiki/MXC
.. _Mazeworld Catacombs: http://macintoshgarden.org/games/mazeworld-catacombs
.. _Scarab of RA: http://macintoshgarden.org/games/scarab-of-ra
.. _NetHack: http://nethack4.org
.. _Rogue: http://en.wikipedia.org/wiki/Rogue_(video_game)
