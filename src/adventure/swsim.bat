@rem Run software simulator in hands-off mode
..\..\tools\slite\slite\bin\Debug\slite.exe ^
    --bram=bootstrap.code ^
    --flash=adventure.bin ^
    --trigger=ffffffff ^
    --noprompt
