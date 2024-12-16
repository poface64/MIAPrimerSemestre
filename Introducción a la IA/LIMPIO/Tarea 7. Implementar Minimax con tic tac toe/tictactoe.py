## Codigo para jugar Tic-Tac-Toe con Minimax ##
# Extraido de https://github.com/vineetjoshi253/TicTacToe-MiniMax
# Adaptado por Angel García Báez

### Para hacer funcionar el programa es necesario cargarlo desde la terminal
### como python tictactoe.py

# Generar la clase Tic-Tac-Toe
class TicTacToe:
    # Definir el tablero de juego y la forma de interactuar
    def __init__(self):
        self.board = [' ' for _ in range(9)]
        self.human = 'X'
        self.computer = 'O'

    # Imprimir el tablero despues de cada jugada
    def print_board(self):
        for i in range(0, 9, 3):
            print(f'{self.board[i]} | {self.board[i+1]} | {self.board[i+2]}')
            if i < 6:
                print('---------')

    # Movimientos disponibles
    def available_moves(self):
        return [i for i, spot in enumerate(self.board) if spot == ' ']

    # Hacer el movimiento del jugador
    def make_move(self, position, player):
        if self.board[position] == ' ':
            self.board[position] = player
            return True
        return False
    # Condición de paro para determinar un ganador
    def check_winner(self, player):
        # Check rows
        for i in range(0, 9, 3):
            if all(self.board[i+j] == player for j in range(3)):
                return True
        # Check columns
        for i in range(3):
            if all(self.board[i+j*3] == player for j in range(3)):
                return True
        # Check diagonals
        if all(self.board[i] == player for i in [0, 4, 8]) or \
           all(self.board[i] == player for i in [2, 4, 6]):
            return True
        return False
    # Verificador de si el tablero esta lleno
    def is_board_full(self):
        return ' ' not in self.board
    # Implementación del algoritmo MIN-MAX para
    # la logica de juego de la computadora.
    def minimax(self, depth, is_maximizing):
        if self.check_winner(self.computer):
            return 1
        if self.check_winner(self.human):
            return -1
        if self.is_board_full():
            return 0

        if is_maximizing:
            best_score = float('-inf')
            for move in self.available_moves():
                self.board[move] = self.computer
                score = self.minimax(depth + 1, False)
                self.board[move] = ' '
                best_score = max(score, best_score)
            return best_score
        else:
            best_score = float('inf')
            for move in self.available_moves():
                self.board[move] = self.human
                score = self.minimax(depth + 1, True)
                self.board[move] = ' '
                best_score = min(score, best_score)
            return best_score
    # Función para determinar el mejor movimiento 
    # Dado el movimiento que hizo el usuario
    def get_best_move(self):
        best_score = float('-inf')
        best_move = None
        for move in self.available_moves():
            self.board[move] = self.computer
            score = self.minimax(0, False)
            self.board[move] = ' '
            if score > best_score:
                best_score = score
                best_move = move
        return best_move
    # Función para la interfaz inicial
    def play_game(self):
        print("Bienvenido al juego del Gato!")
        print("Tu eres la X y la computadora es O")
        print("Las posiciones jugables estan numeradas del 0 al 8")
        
        while True:
            self.print_board()
            
            # Human's turn
            while True:
                try:
                    position = int(input("Enter your move (0-8): "))
                    if 0 <= position <= 8 and self.make_move(position, self.human):
                        break
                    else:
                        print("Invalid move, try again")
                except ValueError:
                    print("Please enter a number between 0-8")
            
            # Check if human won
            if self.check_winner(self.human):
                self.print_board()
                print("Congratulations! You won!")
                break
                
            # Check if board is full
            if self.is_board_full():
                self.print_board()
                print("It's a tie!")
                break
            
            # Computer's turn
            print("\nComputer is making a move...")
            computer_move = self.get_best_move()
            self.make_move(computer_move, self.computer)
            
            # Check if computer won
            if self.check_winner(self.computer):
                self.print_board()
                print("Computer wins!")
                break
                
            # Check if board is full
            if self.is_board_full():
                self.print_board()
                print("It's a tie!")
                break


# Función para inicializar la clase del objeto tic tac toe 
# y comenzar el juego
if __name__ == "__main__":
    game = TicTacToe()
    game.play_game()


