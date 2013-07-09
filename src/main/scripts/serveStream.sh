cvlc /home/josh/testvid.avi --start-time 121 --sout '#transcode{vcodec=h264,vb=800,scale=1,acodec=mpga,ab=128,channels=2,samplerate=44100}:rtp{mux=ts,dst=192.168.1.141,port=5004}'

