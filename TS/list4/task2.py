import random
from collections import defaultdict

# ─────────────────────────────────────────────────────────────────────────────
# CONFIGURATION
# ─────────────────────────────────────────────────────────────────────────────
MEDIUM_LEN    = 140                                 # number of cells in the shared bus
TOTAL_STEPS   = 2000                                # how many discrete time‐steps to run
SIGNAL_LENGTH = 10                                  # how many contiguous cells a data burst covers
JAM_DURATION  = 140                                  # how many steps a station actively emits its own '!' 
BACKOFF_RANGE = (3, 15)   # after any jam (or at startup), random wait
# ─────────────────────────────────────────────────────────────────────────────


class Signal:
    """
    A unified signal class. If is_jam=True, behaves like a jam wave: two heads move outward
    until off‐bus. If is_jam=False, behaves like a data burst: grows to SIGNAL_LENGTH while
    its origin_station is 'transmitting', then propagates as a fixed‐length block. If the
    origin_station is jammed mid‐burst, the burst “freezes” at whatever length it has reached
    and then propagates until off‐bus.
    """
    def __init__(self, origin_station, pos, signal_length, is_jam=False):
        self.origin_station = origin_station  # Station object (or None for jam if desired)
        self.origin_id = origin_station.id if not is_jam else '!'
        self.is_jam = is_jam
        self.length = signal_length if not is_jam else 0
        self.age = 0  # only used for data bursts

        # heads and tails:
        # For both data and jam, we track two heads (pos, direction).
        # For data (is_jam=False), we also track two tails.
        self.heads = [(pos, -1), (pos, +1)]
        if not is_jam:
            self.tails = [(pos, -1), (pos, +1)]
        else:
            self.tails = []  # jam has no tails

    def propagate(self):
        """
        Advance one time‐step:
        - If is_jam: move both heads outward; drop any head off‐bus.
        - Else (data burst):
            * If age < length and origin still transmitting: age += 1; move only heads.
            * If age < length but origin not transmitting: set age=length; move heads+tails.
            * If age >= length: move both heads and tails outward in sync.
        """
        if self.is_jam:
            new_heads = []
            for (p, d) in self.heads:
                new_p = p + d
                if 0 <= new_p < MEDIUM_LEN:
                    new_heads.append((new_p, d))
            self.heads = new_heads
        else:
            # Data burst logic
            if self.age < self.length:
                if self.origin_station.state == 'transmitting':
                    # still building the burst
                    self.age += 1
                    # move heads outward
                    self.heads = [(p + d, d) for (p, d) in self.heads]
                    # tails remain at origin until age == length
                else:
                    # origin was jammed before burst finished: freeze at full length
                    self.age = self.length
                    # move heads and tails outward one cell
                    self.heads = [(p + d, d) for (p, d) in self.heads]
                    self.tails = [(p + d, d) for (p, d) in self.tails]
            else:
                # full‐length packet propagates: move both heads and tails outward
                self.heads = [(p + d, d) for (p, d) in self.heads]
                self.tails = [(p + d, d) for (p, d) in self.tails]

    def occupied_cells(self):
        """
        Return a set of all cells currently occupied by this signal:
        - If is_jam: just both head positions.
        - Else (data): the range from each tail to its corresponding head.
        """
        if self.is_jam:
            return {p for (p, _) in self.heads}
        else:
            occupied = set()
            # For each direction, include all cells from tail to head (inclusive)
            for ((tpos, _), (hpos, _)) in zip(self.tails, self.heads):
                if tpos <= hpos:
                    occupied.update(range(tpos, hpos + 1))
                else:
                    occupied.update(range(hpos, tpos + 1))
            return occupied


class Station:
    """
    Each station cycles through:
      'waiting'  → (wait_timer expires) → 'transmitting'
      'transmitting' → (if collision) → 'jam' (JAM_DURATION)
      'jam' → (jam_timer expires) → 'waiting' (new random backoff)

    While in 'transmitting', it spawns one data‐Signal that grows to SIGNAL_LENGTH (unless cut)
    and then propagates until off‐bus. If jammed mid‐burst, the packet freezes and continues.
    """
    def __init__(self, station_id, position):
        self.id = station_id
        self.pos = position
        self.state = 'waiting'
        self.wait_timer = random.randint(*BACKOFF_RANGE)
        self.jam_timer = 0

    def try_transmit(self):
        if self.state == 'waiting' and self.wait_timer <= 0:
            self.state = 'transmitting'

    def detect_collision(self):
        if self.state == 'transmitting':
            self.state = 'jam'
            self.jam_timer = JAM_DURATION

    def hear_external_jam(self):
        if self.state == 'transmitting':
            self.state = 'waiting'
            self.wait_timer = random.randint(*BACKOFF_RANGE)
        if self.state == 'waiting':
            self.wait_timer = random.randint(*BACKOFF_RANGE)

    def tick(self):
        if self.state == 'jam':
            self.jam_timer -= 1
            if self.jam_timer <= 0:
                self.state = 'waiting'
                self.wait_timer = random.randint(*BACKOFF_RANGE)
        elif self.state == 'waiting':
            self.wait_timer -= 1
            if self.wait_timer <= 0:
                self.try_transmit()


def simulate_csma_cd(
    medium_length,
    stations,
    steps,
    signal_length=SIGNAL_LENGTH
):
    """
    - medium_length: number of cells (int)
    - stations: list of Station objects at fixed positions
    - steps: total simulation steps (int)
    - signal_length: how many cells each data burst spans
    """
    active_signals = []  # List of Signal instances (data or jam)

    for t in range(steps):
        # ─────────────────────────────────────────────────────────────────────
        # 1) Spawn new signals for stations entering 'transmitting' or 'jam'
        # ─────────────────────────────────────────────────────────────────────
        for st in stations:
            if st.state == 'transmitting':
                # If no existing data‐Signal from this station, create one
                if not any(
                    isinstance(sig, Signal)
                    and not sig.is_jam
                    and sig.origin_id == st.id
                    for sig in active_signals
                ):
                    active_signals.append(Signal(st, st.pos, signal_length, is_jam=False))

            elif st.state == 'jam':
                # If no existing jam‐Signal whose head started at this station’s pos, create one
                if not any(
                    isinstance(sig, Signal)
                    and sig.is_jam
                    and st.pos in sig.occupied_cells()
                    for sig in active_signals
                ):
                    active_signals.append(Signal(None, st.pos, 0, is_jam=True))

        # ─────────────────────────────────────────────────────────────────────
        # 2) Build a cell → list(origin_ids) mapping for collision/jam detection
        # ─────────────────────────────────────────────────────────────────────
        cell_map = defaultdict(list)
        for sig in active_signals:
            for c in sig.occupied_cells():
                if 0 <= c < medium_length:
                    cell_map[c].append(sig.origin_id)

        # ─────────────────────────────────────────────────────────────────────
        # 3) Collision detection (data vs. data): any cell with ≥2 distinct station IDs,
        # ignoring cells with jams (jam overrides)
        # ─────────────────────────────────────────────────────────────────────
        collision_cells = [
            c for c, origins in cell_map.items()
            if '!' not in origins and len({oid for oid in origins if oid != '!'}) > 1
        ]
        if collision_cells:
            for st in stations:
                if st.state == 'transmitting' and st.pos in collision_cells:
                    st.detect_collision()

        # ─────────────────────────────────────────────────────────────────────
        # 4) Jam‐hearing: any transmitting station that “sees” a '!' at its own cell
        #    must give up & back off immediately.
        # ─────────────────────────────────────────────────────────────────────
        for st in stations:
            if '!' in cell_map.get(st.pos, []):
                st.hear_external_jam()

        # ─────────────────────────────────────────────────────────────────────
        # 5) (REMOVED) Drop any data‐Signal whose station switched to 'waiting' mid‐step
        #    (stations no longer control signal propagation; signals propagate fully)
        # ─────────────────────────────────────────────────────────────────────
        # skipping step 5 entirely

        # ─────────────────────────────────────────────────────────────────────
        # 6) Propagate every signal (data or jam). If fully off‐bus, drop it.
        # ─────────────────────────────────────────────────────────────────────
        next_signals = []
        for sig in active_signals:
            sig.propagate()
            occupied = sig.occupied_cells()
            if any(0 <= c < medium_length for c in occupied):
                next_signals.append(sig)
        active_signals = next_signals

        # ─────────────────────────────────────────────────────────────────────
        # 7) Tick each station’s timers (jam → waiting → transmit, waiting → transmit)
        # ─────────────────────────────────────────────────────────────────────
        for st in stations:
            st.tick()

        # ─────────────────────────────────────────────────────────────────────
        # 8) (Optional) Print bus state for debugging
        # ─────────────────────────────────────────────────────────────────────
        _print_medium_state(t, medium_length, active_signals, stations)


def _print_medium_state(time_step, length, signals, stations):
    """
    Visual key per cell:
      . = empty
      digit = station ID’s data burst occupies that cell
      ! = jam wave present
      X = ≥2 distinct data‐IDs collided in that cell
      W = station at that cell is in 'waiting' (backoff)
      T = station at that cell is 'transmitting' or 'jam'ing
    """
    cell_contents = [set() for _ in range(length)]
    for sig in signals:
        for c in sig.occupied_cells():
            if 0 <= c < length:
                cell_contents[c].add(sig.origin_id)

    display = []
    for i, contents in enumerate(cell_contents):
        if not contents:
            display.append('·')
        elif '!' in contents and len(contents) == 1:
            display.append('!')
        else:
            non_jams = [oid for oid in contents if oid != '!']
            if len(set(non_jams)) > 1:
                display.append('⨉')
            else:
                display.append(str(next(iter(contents))))

    # Overlay station icons (W=waiting, T=transmitting/jamming)
    for st in stations:
        if 0 <= st.pos < length:
            if st.state == 'waiting':
                display[st.pos] = '[⏳]'
            elif st.state in ('transmitting', 'jam'):
                display[st.pos] = '[📡]'

    print(f"T={time_step:04d}: {''.join(display)}")


# ─────────────────────────────────────────────────────────────────────────────
# Example usage:
# ─────────────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    # Place five stations at different positions on the 150‐cell bus
    stations = [
        Station(1, 10),
        Station(2, 40),
        Station(3, 60),
        Station(4, 80),
        Station(5, 110),
    ]

    # At t=0, all stations are in 'waiting' with random backoffs.
    simulate_csma_cd(
        MEDIUM_LEN,
        stations,
        TOTAL_STEPS,
        signal_length=SIGNAL_LENGTH
    )
