# task2.py

import random
import time

class EthernetMedium:
    def __init__(self, size):
        self.size = size
        self.medium = [None] * size
        self.collision = False
        self.jamming = False

    def transmit(self, sender_id, start_position, data_id):
        """Symuluje transmisję danych przez węzeł."""
        if self.jamming:
            print(f"Węzeł {sender_id}: Medium zajęte (sygnał zagłuszający). Próba później.")
            return False

        # Sprawdzenie, czy medium jest wolne w momencie rozpoczęcia transmisji
        for i in range(start_position, min(start_position + 5, self.size)): # Zakładamy, że transmisja trwa 5 jednostek czasu
            if self.medium[i] is not None:
                print(f"Węzeł {sender_id}: Wykryto zajęte medium przed transmisją.")
                return False

        print(f"Węzeł {sender_id}: Rozpoczyna transmisję danych {data_id} od pozycji {start_position}.")
        collision_occurred = False
        for i in range(start_position, min(start_position + 5, self.size)):
            if self.medium[i] is None:
                self.medium[i] = (sender_id, data_id)
            elif self.medium[i][0] != sender_id:
                print(f"Węzeł {sender_id}: Wykryto kolizję na pozycji {i} (z węzłem {self.medium[i][0]}).")
                self.collision = True
                collision_occurred = True
                break
        return not collision_occurred

    def sense(self, position):
        """Sprawdza stan medium w danej pozycji."""
        return self.medium[position] is None

    def jam(self):
        """Symuluje wysłanie sygnału zagłuszającego."""
        print("Medium: Wykryto kolizję. Wysyłanie sygnału zagłuszającego.")
        self.jamming = True
        self.medium = ['JAM'] * self.size # Oznaczamy całe medium jako zajęte zagłuszaniem

    def clear(self):
        """Czyści medium po transmisji lub zagłuszaniu."""
        self.medium = [None] * self.size
        self.collision = False
        self.jamming = False

    def get_state(self):
        """Zwraca aktualny stan medium."""
        return list(map