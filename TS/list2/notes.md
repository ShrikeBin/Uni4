## Some Notes
```bash
### check for spoofing:
sniff(iface="wlan0", prn=lambda x: x.pdst if ARP in x else None)
```