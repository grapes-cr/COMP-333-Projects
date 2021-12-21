

public class EightQueens {
	
	private static boolean isSafe(int board[][], int row, int col) {
		for (int i = 0; i < col; i++) {
			if (board[row][i] == 1) {  //if theres already a queen on the left return false 
				return false;
			}
		}
		for (int i = row, j = col; i >= 0 && j >= 0; i--, j--) {
			if (board[i][j] == 1) { //check diag upleft
				return false;
			}
		}
		for (int i = row, j = col; i>=0 && j<board[0].length; i--, j++) {
			if (board[i][j] == 1) { //check diag downleft
				return false;
			}
		}
		return true; //else true (spot is safe)
	}
	
	private static void printBoard(int board[][]) {
		for (int i = 0; i < board[0].length; i++) {
			for (int j = 0; j < board[0].length; j++) {
				System.out.print(board[i][j] + " ");
			}
			System.out.println();
		}
	}
	
	private static boolean solveBoard(int board[][], int col) {
		if (col >= board[0].length) { //base case: if end of board, exit
			return true;
		}
		for (int i = 0; i < board[0].length; i++) {
			if (isSafe(board, i, col)) {
				board[i][col] = 1; //if all requirements are met, place queen then recurse with next col
				if(solveBoard(board, col + 1)) { 
					return true;
				}
				board[i][col] = 0;
			}
		}
		return false;
	}
	
	public static void main(String[] args) {
		int board[][] = {{0,0,0,0}, 
						 {0,0,0,0},
						 {0,0,0,0},
						 {0,0,0,0}};
		if (solveBoard(board, 0) == false) {
            System.out.print("board is empty, solution DNE.");
        }
		printBoard(board);		 
	}

}
