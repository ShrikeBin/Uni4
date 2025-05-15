FLAG_SEQUENCE = "01111110"
BIT_STUFFING_THRESHOLD = 5
CRC_POLYNOMIAL = "10001000000100001"  # CRC-16 polynomial (17 bits)
FRAME_SIZE = 128  # WITH NO DATA FRAME TAKES 32 BITS

def calculate_crc(data, polynomial):
    poly_len = len(polynomial)
    padded_data = data + '0' * (poly_len - 1)
    remainder = int(padded_data, 2)
    divisor = int(polynomial, 2)

    for i in range(len(data)):
        bit_pos = len(padded_data) - 1 - i
        if (remainder >> bit_pos) & 1:
            remainder ^= divisor << (bit_pos - (poly_len - 1))

    crc = bin(remainder)[2:].zfill(poly_len - 1)
    return crc

def bit_stuffing(data):
    stuffed_data = ""
    consecutive_ones = 0
    for bit in data:
        stuffed_data += bit
        if bit == '1':
            consecutive_ones += 1
            if consecutive_ones == BIT_STUFFING_THRESHOLD:
                stuffed_data += '0'
                consecutive_ones = 0
        else:
            consecutive_ones = 0
    return stuffed_data

def bit_unstuffing(data):
    unstuffed_data = ""
    consecutive_ones = 0
    i = 0
    while i < len(data):
        bit = data[i]
        unstuffed_data += bit
        if bit == '1':
            consecutive_ones += 1
            if consecutive_ones == BIT_STUFFING_THRESHOLD:
                i += 1  # skip stuffed zero after 5 ones
                consecutive_ones = 0
        else:
            consecutive_ones = 0
        i += 1
    return unstuffed_data

def frame_data(data):
    frames = []
    crc_len = len(CRC_POLYNOMIAL) - 1
    max_payload = FRAME_SIZE - 2 * len(FLAG_SEQUENCE) - crc_len

    # Conservative estimate: leave some margin for stuffing overhead (if happens all of the chunk is 1s)
    estimated_growth_factor = 1.2
    safe_chunk_size = max_payload + crc_len

    while data:
        chunk = data[:safe_chunk_size]
        data = data[safe_chunk_size:]

        crc = calculate_crc(chunk, CRC_POLYNOMIAL)
        chunk_with_crc = chunk + crc
        stuffed_data = bit_stuffing(chunk_with_crc)

        # 
        if len(stuffed_data) > max_payload:
            # back off and retry with smaller chunk
            safe_chunk_size = int(safe_chunk_size/estimated_growth_factor)
            data = chunk + data  # put data back
            continue

        frame = FLAG_SEQUENCE + stuffed_data + FLAG_SEQUENCE
        frames.append(frame)

    return frames


def deframe_data(frame, frame_id):
    if not frame.startswith(FLAG_SEQUENCE) or not frame.endswith(FLAG_SEQUENCE):
        print(f"Błąd: Nieprawidłowa ramka {frame_id} (brak flagi).")
        return None

    stuffed_data = frame[len(FLAG_SEQUENCE):-len(FLAG_SEQUENCE)]
    unstuffed_data = bit_unstuffing(stuffed_data)

    crc_len = len(CRC_POLYNOMIAL) - 1
    received_data = unstuffed_data[:-crc_len]
    received_crc = unstuffed_data[-crc_len:]

    calculated_crc = calculate_crc(received_data, CRC_POLYNOMIAL)
    if calculated_crc == received_crc:
        return received_data
    else:
        print(f"Błąd CRC dla ramki {frame_id}: Otrzymano {received_crc}, obliczono {calculated_crc}")
        return None

if __name__ == "__main__":
    source_file = 'in.txt'
    framed_file = 'out.txt'

    try:
        with open(source_file, 'r') as f_in:
            original_data = f_in.read().strip()
        print(f"Dane z {source_file} : \n|{original_data}|")

        framed_results = frame_data(original_data)

        with open(framed_file, 'w') as f_out:
            for i, framed_result in enumerate(framed_results):
                f_out.write(framed_result + "\n")
                print(f"Ramka {i + 1}: |{framed_result}| (długość: {len(framed_result)} bitów) zapisana do {framed_file}")

        if input(f"Wczytać ramki z {framed_file}? (y/n): ").lower() not in ('y', 'yes'): exit()

        with open(framed_file, 'r') as f_in:
            received_frames = f_in.readlines()

        unframed_data = ""
        i = 0
        for received_frame in received_frames:
            i+=1
            received_frame = received_frame.strip()
            unframed_result_part = deframe_data(received_frame,i)
            if unframed_result_part is None:
                unframed_data = None
                break
            unframed_data += unframed_result_part

        if unframed_data is not None:
            print(f"Odramkowane dane: \n|{unframed_data}|")
            if unframed_data == original_data:
                print("Weryfikacja pomyślna: Dane odzyskane poprawnie.")
            else:
                print("Błąd w weryfikacji: Odramkowane dane różnią się od oryginalnych.")
        else:
            print("Weryfikacja nie powiodła się.")

    except FileNotFoundError:
        print(f"Błąd: Nie można znaleźć pliku {source_file}.")
    except Exception as e:
        print(f"Wystąpił nieoczekiwany błąd: {e}")
