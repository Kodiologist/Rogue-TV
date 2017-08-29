Rogue TV is a nonviolent roguelike where you play a lucky contestant on a roguelike-themed game show. It draws inspiration from `Smash TV`_, `Weekend Warrior`_, MXC_, `Legends of the Hidden Temple`_, `Mazeworld Catacombs`_, `Scarab of RA`_, NetHack_, and, of course, Rogue_. It's written in Hy_ with Python 3. It runs on both CPython and PyPy; you can get better performance, especially for map generation, on PyPy.

.. figure:: http://i.imgur.com/vcPgIAK.png
  :alt: Screenshot of an ASCII roguelike

  Running in GNOME Terminal on Linux.

Currently, the game is playable, but a lot of features remain to be implemented.

Dependencies include Hy_ (``3db13ec71f2c79a1b91f3e0a7169d85658a410a1``), Kodhy_, Heidegger_, and curses. (All dependencies not already included with Python are in pure Python or Hy.) You can download the game and its dependencies as a self-contained bundle for `Unix-likes (POSIX-compliant systems)`_ or `Windows (64-bit only)`_. The POSIX bundle has been tested with Linux and Mac OS X, as well as Windows with Cygwin_ and Cygwin's Python installed. The Windows bundle includes a subset of Cygwin and Python so you don't have to install these first. I keep both bundles up to date with the master branch on GitHub.

.. _Unix-likes (POSIX-compliant systems): http://arfer.net/downloads/roguetv-posix.tar.gz
.. _Windows (64-bit only): http://arfer.net/downloads/roguetv-windows.tar.gz

To start the game:

- From the POSIX bundle, type ``sh run.sh``.
- From the Windows bundle, double-click ``run.bat`` (its name may appear as ``run`` if you have file extensions hidden) to get a command window, then type ``sh run.sh`` and hit Enter.
- From the source code, type ``hy run.hy``.

Use the ``--help`` argument to see command-line options. The game requires a terminal emulator that supports 256 colors and Unicode, such as GNOME Terminal, Konsole, LXTerminal, Terminal.app, or Mintty. It works fine over SSH or in screen or tmux.

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
