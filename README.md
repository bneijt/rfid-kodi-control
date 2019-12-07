# rfid-kodi-control

I bought [a cheap RFID scanner](https://www.aliexpress.com/item/4000038621078.html?spm=a2g0s.9042311.0.0.79(3d4c4dUtRvUK) that presents itself as a USB HID device (keyboard)
when plugged in.

Then I wrote a small Haskell program that will claim the keyboard (make sure no other applications get the read numbers) and then send a command to [Kodi](https://kodi.tv/) to start playing something
connected with the cards.

Now my son can easily pick which media he wants to play, without giving him access to the whole media library (aka, the remote control).

TODO explain installation and approach.
