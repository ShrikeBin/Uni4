# CSMA/CD Simulator - Prosty model w Pythonie
import random

class Station:
    def __init__(self, position, p_send):
        self.position = position    # indeks na medium
        self.p_send = p_send        # prawdopodobieństwo wysyłki
        self.transmitting = False   # czy aktualnie nadaje
        self.backoff = 0            # sloty do odczekania przy kolizji
        self.frame_time = 0         # pozostałe sloty ramki
        self.success = 0            # licznik udanych transmisji
        self.collisions = 0         # licznik kolizji

    def start_transmission(self):
        self.transmitting = True
        self.frame_time = 3  # ramka trwa 3 sloty

    def handle_collision(self):
        self.transmitting = False
        self.collisions += 1
        self.backoff = random.randint(1, 4)

    def tick(self, medium):
        # odczekanie backoffu
        if self.backoff > 0:
            self.backoff -= 1
            return

        # rozpoczęcie nowej ramki
        if not self.transmitting and random.random() < self.p_send:
            # stacja sprawdza, czy na jej pozycji nie ma sygnału
            if medium[self.position] == 0:
                self.start_transmission()
                medium[self.position] += 1
                return

        # kontynuacja transmisji (jeśli już rozpoczęta)
        if self.transmitting:
            medium[self.position] += 1

    def finish(self, medium):
        if self.transmitting:
            # sprawdzamy kolizję na pozycji stacji
            if medium[self.position] > 1:
                self.handle_collision()
            else:
                # zmniejszamy czas ramki
                self.frame_time -= 1
                if self.frame_time == 0:
                    self.transmitting = False
                    self.success += 1

class Medium:
    def __init__(self, length):
        self.length = length
        self.cells = [0] * length

    def reset(self):
        self.cells = [0] * self.length

    def propagate(self):
        # sygnał rozchodzi się do sąsiadów
        new = [0] * self.length
        for i, val in enumerate(self.cells):
            if val > 0:
                # sygnał w bieżącej komórce
                new[i] += val
                # do sąsiadów
                if i > 0:
                    new[i-1] += val
                if i < self.length - 1:
                    new[i+1] += val
        self.cells = new

class Simulator:
    def __init__(self, num_stations=10, length=50, p_send=0.1, slots=1000):
        # rozmieszczamy stacje równomiernie
        step = max(1, length // num_stations)
        positions = [i * step for i in range(num_stations)]
        self.stations = [Station(pos, p_send) for pos in positions]
        self.medium = Medium(length)
        self.slots = slots

    def run(self):
        for _ in range(self.slots):
            # reset medium na początek slotu
            self.medium.reset()

            # każda stacja wrzuca sygnał (start/continuation)
            for st in self.stations:
                st.tick(self.medium.cells)

            # propagacja sygnału w medium
            self.medium.propagate()

            # zakończenie slotu: obsługa sukcesu/kolizji
            for st in self.stations:
                st.finish(self.medium.cells)

        total_success = sum(s.success for s in self.stations)
        total_coll = sum(s.collisions for s in self.stations)
        throughput = total_success / self.slots
        return {
            'total_success': total_success,
            'total_collisions': total_coll,
            'throughput': throughput
        }

if __name__ == '__main__':
    sim = Simulator(num_stations=2, length=4, p_send=0.9, slots=100)
    results = sim.run()
    print("Wyniki symulacji CSMA/CD:")
    print(f"  Udane transmisje: {results['total_success']}")
    print(f"  Kolizje: {results['total_collisions']}")
    print(f"  Przepustowość (ramki/slot): {results['throughput']:.5f}")
