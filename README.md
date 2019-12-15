rfid-kodi-control
======
I bought [a cheap RFID scanner](https://www.aliexpress.com/item/4000038621078.html) that presents itself as a USB HID device (keyboard)
when plugged in.

Then I wrote a small Haskell program that will claim the keyboard (make sure no other applications get the read numbers) and then send a command to [Kodi](https://kodi.tv/) to start playing something
connected with the cards.

Now my son can easily pick which media he wants to play, without giving him access to the whole media library (aka, the remote control).

Installation
====
Step 1. Use udev to get a normal user direct access
-------
First we need to be able to access and _grab_ the device as a normal user. _Grabbing_ means that any
input the device gives goes only to the program we run and not a left open terminal or something like that.

Using `udevd` we change the owner of the input device to the user running this haskell program (do not run this as root, be sensible). For me that user is `bram`.

I created a file called `/etc/udev/rules.d/99-reader.rules` and put in:

    SUBSYSTEM=="input", ACTION=="add", ATTRS{uniq}=="08FF20140315", OWNER="bram"

I got the `ATTRS{uniq}` part from, after plugging in the scanner and using `ls -alh /dev/input/by-id` to find out which `/dev/input/event...` the device was connect to.

Once you know that, you can get matching attributes for this udev rule by using `udevadm info -a /dev/input/event20` (replacing the `20` with the number you found out it was connected to when you plugged it in).

After creating the file, restart the `udev` daemon to make sure the configuration is picked up:

    sudo systemctl restart systemd-udevd

Step 2. Build the application
------------
The application is developed using [stack](https://github.com/commercialhaskell/stack) which is installable in most Linux distributions. After installing this build tool, simply run:

    stack build

to build the program.

Step 3. Configure the application
---------

Configuration is done using the `rdif-kodi.yaml`, you can start with `mv rfid-kodi-example.yaml rfid-kodi.yaml` to get a template, see [rfid-kodi-example.yaml](rfid-kodi-example.yaml).
Each card number maps to a list of options to play. Every time a card is scanned it will randomly select an element from the list.

Step 4. Run the application
----------
Run the application by simply using stack to execute the compiled binary (see also [run.sh](run.sh)):

    stack exec rfid-kodi-control-exe

At this point you should have a working scanner that sends a command to kodi using the information from the configuration file.

Roadmap
------
The application can use a lot of polishing, but I also have other projects to attend to so this will probably sit here till I need changes or somebody creates an issue or sends me a friendly email.
