
# AirPods

Command line utility for reconnecting AirPods and setting appropriate volume levels. Sets all playing applications
(e.g. Firefox, Spotify, VLC) to send audio to the AirPods.

Avoids the manual process of re-connecting and setting outputs using Blueman and pavucontrol.

Inspired by:

https://askubuntu.com/questions/833779/pairing-bluetooth-headphones-in-command-line

https://askubuntu.com/questions/71863/how-to-change-pulseaudio-sink-with-pacmd-set-default-sink-during-playback

https://www.freedesktop.org/wiki/Software/PulseAudio/FAQ/#howdoiswitchthedefaultsoundcardmovingallapplications

Setting the internal volume of the AirPods requires a patched version of bluez: https://carlo-hamalainen.net/2021/05/20/airpods

Use ``bt-device -l`` to find MAC addresses of your Bluetooth devices.

Usage:

    airpods --mac=12:34:56:78:9A:BC

Example:

    $ airpods --mac=12:34:56:78:9A:BC
    Attempting to disconnect from 12:34:56:78:9A:BC
    [CHG] Device 12:34:56:78:9A:BC ServicesResolved: no
    Successful disconnected
    [CHG] Device 12:34:56:78:9A:BC Connected: no
    Attempting to connect to 12:34:56:78:9A:BC
    [CHG] Device 12:34:56:78:9A:BC Connected: yes
    Connection successful
    ("move-sink-input",356,"==>",86)
    method return time=1629588668.603664 sender=:1.1217 -> destination=:1.1936 serial=4938 reply_serial=2
