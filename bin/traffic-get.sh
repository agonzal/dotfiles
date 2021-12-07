#!/bin/bash

# Set the default values or Read the users input
[ -z "${1}" ] && IFACE="eth0"  || IFACE="$1"       # Get the name of the target interface, default: eth0
[ -z "${2}" ] && UNIT="MB"     || UNIT="$2"        # Get the unit (B, KB, MB, GB, Kib, Mib, Gib), default: MB
[ -z "${3}" ] && PERIOD="30"   || PERIOD="$3"      # Get the period of measure in seconds, default: 30
[ -z "${4}" ] && OUTPUT="verb" || OUTPUT="${4,,}"  # Get the type of the output (verbose, all, incoming, outgoing, total) in lower case, default: all
LANG=C # Set envvar $LANG to `C` due to grep, awk, etc.

# Do the conversion
if   [ "$UNIT" == "B"   ]; then UN="1"
elif [ "$UNIT" == "KB"  ]; then UN="1000"
elif [ "$UNIT" == "KiB" ]; then UN="1024"
elif [ "$UNIT" == "MB"  ]; then UN="1000000"
elif [ "$UNIT" == "MiB" ]; then UN="1048576"
elif [ "$UNIT" == "GB"  ]; then UN="1000000000"
elif [ "$UNIT" == "GiB" ]; then UN="1073741824"
else echo "Wrong UNIT."; exit 1; fi

# Whether the $PERIOD is integer
if ! [[ "$PERIOD" =~ ^[0-9]+$ ]]; then echo "Enter the PERIOD in seconds"; exit 1; fi

# Get the IP address of the interface
get_ip(){ /sbin/ifconfig "$IFACE" 2>/dev/null | grep -Po '[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+' | head -1; }

# The main program: If the interface has IP adders it is UP
if [[ "$(get_ip)" =~ ${IPPT} ]]; then
        #bRX="$(< /sys/class/net/"$IFACE"/statistics/rx_bytes)"
        #bRX="$(< /sys/class/net/"$IFACE"/statistics/tx_bytes)"
        bRX="$(/sbin/ifconfig "$IFACE" | grep -Po "RX bytes:[0-9]+" | sed 's/RX bytes://')" # Get the incoming traffic into the Beginning of the period
        bTX="$(/sbin/ifconfig "$IFACE" | grep -Po "TX bytes:[0-9]+" | sed 's/TX bytes://')" # Get the outgoing traffic into the Beginning of the period
        bXX=$(( bRX + bTX )) # Calculate the total traffic into the Beginning of the PERIOD

        sleep "$PERIOD" # Sleep for the PERIOD, seconds

        #bRX="$(< /sys/class/net/"$IFACE"/statistics/rx_bytes)"
        #bRX="$(< /sys/class/net/"$IFACE"/statistics/tx_bytes)"
        eRX="$(/sbin/ifconfig "$IFACE" | grep -Po "RX bytes:[0-9]+" | sed 's/RX bytes://')" # Get the incoming traffic into the End of the period
        eTX="$(/sbin/ifconfig "$IFACE" | grep -Po "TX bytes:[0-9]+" | sed 's/TX bytes://')" # Get the outgoing traffic into the End of the period
        eXX=$(( eRX + eTX )) # Calculate the total traffic into the End of the PERIOD

        #RX=$(( ( eRX - bRX ) / UN )) # Calculate the amount of the incoming traffic for the PERIOD
        #TX=$(( ( eTX - bTX ) / UN )) # Calculate the amount of the outgoing traffic for the PERIOD
        #XX=$(( ( eXX - bXX ) / UN )) # Calculate the amount of the total traffic for the PERIOD
        RX=$(awk -v e="${eRX}" -v b="${bRX}" -v un="${UN}" 'BEGIN{ print ( e - b) / un }') # Calculate the amount of the incoming traffic for the PERIOD
        TX=$(awk -v e="${eTX}" -v b="${bTX}" -v un="${UN}" 'BEGIN{ print ( e - b) / un }') # Calculate the amount of the outgoing traffic for the PERIOD
        XX=$(awk -v e="${eXX}" -v b="${bXX}" -v un="${UN}" 'BEGIN{ print ( e - b) / un }') # Calculate the amount of the total traffic for the PERIOD

        # Output
        if   [[ "$OUTPUT" =~ ^verb ]]; then printf 'Interface: %s\nUnit: %s\nPeriod of measure: %s sec.\n\nReceived: %s\nTransmited: %s\nTotal: %s\n' "$IFACE" "$UNIT" "$PERIOD" "$RX" "$TX" "$XX"
        elif [[ "$OUTPUT" =~ ^all  ]]; then printf '%s\n%s\n%s\n' "$RX" "$TX" "$XX"
        elif [[ "$OUTPUT" =~ ^in   ]]; then printf '%s\n' "$RX"
        elif [[ "$OUTPUT" =~ ^out  ]]; then printf '%s\n' "$TX"
        elif [[ "$OUTPUT" =~ ^tot  ]]; then printf '%s\n' "$XX"
        else echo "Wrong OTPUT type."; fi
else
        echo "The INTERFACE \"$IFACE\" is down."
fi
