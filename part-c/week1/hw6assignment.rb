# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces.push(rotations([[0,0], [1,0], [0,1], [1,1], [2,1]]))
                            .push(rotations([[0,0], [1,0], [2,0], [3,0], [4,0]]))
                            .push(rotations([[0,0], [0,1], [1,1]]))
  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def num_blocks
    @all_rotations[0].size
  end
end

class MyBoard < Board

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheating_atm = false
  end

  def move_hundred_eighty
    if is_allowed
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def attempt_cheat
    if @score >= 100 and not @cheating_atm
      @score -= 100
      @cheating_atm = true
    end
  end

  def next_piece
    if @cheating_atm
      @current_block = MyPiece.new([[[0,0]]], self)
      @cheating_atm = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def is_allowed
     !game_over? and @game.is_running?
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0...(@current_block.num_blocks)).each{|index|  # notice change
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris
  # your enhancements here

  def key_bindings
    super
    @root.bind('u', proc {@board.move_hundred_eighty})
    @root.bind('c', proc {@board.attempt_cheat})
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
end
