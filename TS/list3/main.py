import networkx as nx
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.gridspec import GridSpec


# Generowanie grafu G
def generate_graph():
    while True:
        G = nx.gnm_random_graph(n=20, m=29)
        if nx.is_connected(G):
            return G

# Generowanie macierzy natężeń N
def generate_N(size=20, min_val=10, max_val=50):
    N = np.random.randint(min_val, max_val + 1, size=(size, size))
    np.fill_diagonal(N, 0)
    return N

# Przypisywanie przepustowości c i przepływów a
def assign_edge_functions(G, N, packet_size=1000):
    c = {}
    a = {}
    for e in G.edges():
        c[e] = np.random.randint(10_000, 1_000_000)  # bits/s

    for i in range(len(N)):
        for j in range(len(N)):
            if N[i][j] > 0:
                try:
                    path = nx.shortest_path(G, source=i, target=j)
                    for k in range(len(path) - 1):
                        edge = tuple(sorted((path[k], path[k + 1])))
                        a[edge] = a.get(edge, 0) + N[i][j]
                except nx.NetworkXNoPath:
                    continue

    for e in G.edges():
        a[e] = a.get(e, 0)

    for e in G.edges():
        while c[e] <= a[e] * packet_size:
            c[e] *= 2

    return c, a

# Symulacja niezawodności sieci
def simulate_reliability(G, c, a, N, T_max, p, packet_size=1000, trials=1000):
    G_total = np.sum(N)
    success = 0

    for _ in range(trials):
        G_copy = G.copy()
        for e in list(G.edges()):
            if np.random.rand() > p:
                G_copy.remove_edge(*e)

        if not nx.is_connected(G_copy):
            continue

        T = 0
        for e in G_copy.edges():
            ae = a[e]
            ce = c[e]
            T += ae / ((ce / packet_size) - ae)
        T /= G_total

        if T < T_max:
            success += 1

    return success / trials

# Wybór najlepszego spośród k wygenerowanych układów
def best_configuration(k=10, T_max=0.05, p=0.95):
    best_reliability = -1
    best_data = None

    for i in range(k):
        G = generate_graph()
        N = generate_N()
        c, a = assign_edge_functions(G, N)
        reliability = simulate_reliability(G, c, a, N, T_max=T_max, p=p)
        print(f"[{i + 1}/{k}] Niezawodność: {reliability:.4f}")

        if reliability > best_reliability:
            best_reliability = reliability
            best_data = (G, N, c, a)

    return best_data, best_reliability

# Zapis grafu i statystyk do pliku PNG z tabelką
def save_graph_with_table(G, N, c, a, reliability, T_max ,filename="best_network.png"):
    fig = plt.figure(figsize=(16, 10))
    gs = GridSpec(1, 2, width_ratios=[1.2, 2])

    # Tabelka z danymi a i c
    edge_data = [(str(e), a[e], c[e]) for e in G.edges()]
    df = pd.DataFrame(edge_data, columns=["Krawędź", "a (pakiety/s)", "c (bity/s)"])
    ax0 = fig.add_subplot(gs[0])
    ax0.axis('off')
    table = ax0.table(cellText=df.values,
                      colLabels=df.columns,
                      loc='center',
                      cellLoc='center')
    table.auto_set_font_size(False)
    table.set_fontsize(8)
    table.scale(1.2, 1.2)

    # Wykres sieci
    ax1 = fig.add_subplot(gs[1])
    pos = nx.spring_layout(G, seed=42)
    nx.draw(G, pos, ax=ax1, with_labels=True, node_color='skyblue', node_size=700, edge_color='gray')
    ax1.set_title(f"Topologia sieci | Niezawodność: {reliability:.4f} | T_max: {T_max}")
    ax1.axis('off')

    plt.tight_layout()
    plt.savefig(filename, dpi=300)
    plt.close()

    # Zapis macierzy N do czytelnego CSV
    size = N.shape[0]
    labeled_N = pd.DataFrame(N, columns=[f"v({j})" for j in range(size)],
                                index=[f"v({i})" for i in range(size)])
    labeled_N.to_csv("best_N.csv", sep='|', encoding='utf-8')


def test_reliability_vs_traffic(G, N, c, a, T_max=0.02, p=0.95, steps=50):
    results = []
    for factor in np.linspace(1, 5, steps):
        N_scaled = (N * factor).astype(int)
        reliability = simulate_reliability(G, c, a, N_scaled, T_max, p)
        results.append((factor, reliability))
    plot_results(results, 'Skalowanie natężeń (x)', 'Niezawodność', 'Niezawodność vs Skalowanie natężeń')
    return results

def test_reliability_vs_capacity(G, N, c, a, T_max=0.02, p=0.95, steps=50):
    results = []
    for factor in np.linspace(1, 5, steps):
        c_scaled = {e: int(cap * factor) for e, cap in c.items()}
        reliability = simulate_reliability(G, c_scaled, a, N, T_max, p)
        results.append((factor, reliability))
    plot_results(results, 'Skalowanie przepustowości (x)', 'Niezawodność', 'Niezawodność vs Skalowanie przepustowości')
    return results

def test_reliability_vs_topology(G_init, N, c_init, a_init, T_max=0.02, p=0.95, steps=20):
    G = G_init.copy()
    c = c_init.copy()
    a = a_init.copy()
    pos = list(G.nodes())
    avg_capacity = int(np.mean(list(c.values())))
    added = 0
    results = []

    while added < steps:
        i, j = np.random.choice(pos, 2, replace=False)
        if not G.has_edge(i, j):
            G.add_edge(i, j)
            edge = tuple(sorted((i, j)))
            c[edge] = avg_capacity
            a[edge] = 0
            reliability = simulate_reliability(G, c, a, N, T_max, p)
            results.append((G.number_of_edges(), reliability))
            added += 1
    plot_results(results, 'Liczba krawędzi', 'Niezawodność', 'Niezawodność vs Zmiany topologii')
    return results

def plot_results(results, xlabel, ylabel, title):
    x_vals, y_vals = zip(*results)
    plt.figure(figsize=(8, 5))
    plt.plot(x_vals, y_vals, marker='o', linestyle='-', color='tab:blue')
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(title)
    plt.grid(True)
    plt.tight_layout()
    plt.savefig(f"{title.replace(' ', '_').lower()}.png", dpi=300)
    plt.close()


if __name__ == "__main__":
    (G, N, c, a), reliability = best_configuration(k=150, T_max=0.02, p=0.95)
    print(f"Najlepsza niezawodność: {reliability:.4f}")

    save_graph_with_table(G, N, c, a, reliability, T_max=0.02)
    print("Wizualizacja z tabelą zapisana do 'best_network.png'")
    print("Macierz natężeń zapisana do 'best_N.csv'")

    print("Uruchamianie testów niezawodności...")

    test_reliability_vs_traffic(G, N, c, a)
    print("Zapisano wykres: niezawodność vs skalowanie natężeń")

    test_reliability_vs_capacity(G, N, c, a)
    print("Zapisano wykres: niezawodność vs skalowanie przepustowości")

    test_reliability_vs_topology(G, N, c, a)
    print("Zapisano wykres: niezawodność vs zmiany topologii")
