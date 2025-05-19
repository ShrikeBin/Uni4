## Maciej Gębala
## 2025-04-13

### Pliki:

- Makefile
- board.h&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;// zasady i obsługa gry
- game_server.c&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;// serwer gry 
- game_client.c&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;// klient dla gry
- game_random_bot.c&nbsp;// losowy bot dla gry

### Wymagania:

- Kompilator: gcc version 13.3.0
- Biblioteka: gsl 2.7 (game_random_bot - generator liczb losowych)
-   ```bash
    sudo apt install gcc libgsl-dev
    ```

### Wywołanie serwera:

```bash
 ./game_server <numer ip> <numer portu>
 ```

### Wywołanie klienta dla człowieka:

```bash
./game_client <numer ip> <numer portu> <gracz> <nick>
```

### Wywołanie losowego bot-a:

```bash
./game_random_bot <numer ip> <numer portu> <gracz> <nick>
```

**gracz == 1 dla **X** <br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;2 dla O.** <br>
**nick == identyfikator (nazwa) gracza (do 9 znaków)**

