# -*- coding: utf-8 -*-

import StringIO
import socket
import sys
import tty
import termios
import os
import select
import fcntl

def send(fo, message):
	fo.write(message + "\n")
	fo.flush()

WIN_WIDTH = 64
WIN_HEIGHT = 32
HOST = "localhost"
PORT = 1029

class Game:
	def __init__(self, view, view_width, view_height, win_width, win_height, win_x, win_y, last_action_x, last_action_y, s):
		self.view = view
		self.view_width = view_width
		self.view_height = view_height
		self.win_width = win_width
		self.win_height = win_height
		self.win_x = win_x
		self.win_y = win_y
		self.last_action_x = last_action_x
		self.last_action_y = last_action_y
		self.s = s

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((HOST, PORT))

buff = StringIO.StringIO(2048)	  # Some decent size, to avoid mid-run expansion

def recv_line(sock):
	while True:
			data = sock.recv(1)
			buff.write(data)
			if '\n' in data:
				value = buff.getvalue()
				buff.truncate(0)
				return value

# Lecture des parametres du jeu.
d = recv_line(s)
#TODO
print >> sys.stderr, d.strip()
d = d.strip().split(" ")
assert(len(d) == 3 and d[0] == "parameters")
d[1] = d[1].split("=")
d[2] = d[2].split("=")
assert(len(d[1]) == 2 and d[1][0] == "width" and len(d[2]) == 2 and d[2][0] == "height")
WIDTH = int(d[1][1])
HEIGHT = int(d[2][1])
# Creation de la vue et du jeu.
view = [ [ (True, 'N') for _ in range(HEIGHT) ] for _ in range(WIDTH) ]
game = Game(view, WIDTH, HEIGHT, WIN_WIDTH, WIN_HEIGHT, 0, 0, WIDTH / 2, HEIGHT / 2, s)

tile = \
	{ (True,  'N'): ("",		     " ")
	, (True,  'X'): ("",		     " ")
	, (False, 'N'): ("",		     " ")
	, (False, 'X'): ("",		     " ")
	, (True,  '0'): ("\033[38;2;0;0;150m",   "~")
	, (False, '0'): ("\033[38;2;50;50;50m",  "~")
	, (True,  '1'): ("\033[38;2;0;150;0m",   "♣")
	, (False, '1'): ("\033[38;2;50;50;50m",  "♣")
	, (True,  '2'): ("",		     ".")
	, (False, '2'): ("\033[38;2;50;50;50m",  ".")
	, (True,  '3'): ("",		     "^")
	, (False, '3'): ("\033[38;2;50;50;50m",  "^")
	, (True,  '4'): ("\033[48;2;75;75;75m",  "%")
	, (False, '4'): ("\033[38;2;50;50;50m",  "%")
	, (True,  '5'): ("",		     ".")
	, (False, '5'): ("\033[38;2;50;50;50m",  ".")
	, (True,  '6'): ("\033[48;2;75;0;0m",    "#")
	, (False, '6'): ("\033[38;2;50;50;50m",  "#")
	, (True,  '7'): ("\033[38;2;75;0;0m",    "+")
	, (False, '7'): ("\033[38;2;50;50;50m",  "+")
	, (True,  '@'): ("\033[38;2;0;0;200m",   "@")
	, (False, '@'): ("\033[38;2;50;50;50m",  "@")
	, (True,  'h'): ("\033[38;2;200;200;0m", "😈")
	, (False, 'h'): ("",		     " ")
	, (True,  'f'): ("\033[38;2;200;200;0m", "🍕")
	, (False, 'f'): ("",		     " ")
	}

def init_stdin():
	global oldtty
	oldtty = termios.tcgetattr(sys.stdin)
	tty.setraw(sys.stdin.fileno())
	tty.setcbreak(sys.stdin.fileno())

def restore_stdin():
	global oldtty
	termios.tcsetattr(sys.stdin, termios.TCSADRAIN, oldtty)

def getch():
	if select.select([sys.stdin], [], [], 0) == ([sys.stdin], [], []):
		return sys.stdin.read(1)
	else:
		return '|'

def move_to(i, j):
	sys.stdout.write("\033[%d;%dH" % (j, i))

def update_view(game, x, y):
	move_to(x - game.win_x, y - game.win_y)
	style, char = tile[ game.view[x][y] ]
	sys.stdout.write(style + char + "\033[39m\033[49m")

def print_view(game):
	for i in range(game.win_width):
		for j in range(game.win_height):
			update_view(game, game.win_x + i, game.win_y + j)

def do_camera(game, c):
	if c == 'W':
		game.win_y = max(0, game.win_y - 1)
		print_view(game)
	elif c == 'S':
		game.win_y = min(game.view_height - game.win_height - 1, game.win_y + 1)
		print_view(game)
	elif c == 'A':
		game.win_x = max(0, game.win_x - 1)
		print_view(game)
	elif c == 'D':
		game.win_x = min(game.view_width - game.win_width - 1, game.win_x + 1)
		print_view(game)
	elif c == 'c':
		game.win_x = min(game.view_width - game.win_width - 1, max(game.last_action_x - game.win_width / 2, 0))
		game.win_y = min(game.view_height - game.win_height - 1, max(game.last_action_y - game.win_height / 2, 0))
		print_view(game)

def do_action(game, d):
	assert(len(d) == 6)
	assert(len(d[1]) == 2 and d[1][0] == "x")
	assert(len(d[2]) == 2 and d[2][0] == "y")
	assert(len(d[3]) == 2 and d[3][0] == "character_index")
	assert(len(d[4]) == 2 and d[4][0] == "hit_point")
	assert(len(d[5]) == 2 and d[5][0] == "symbol")
	game.last_action_x = int(d[1][1])
	game.last_action_y = int(d[2][1])

	move_to(0, game.win_height + 2)
	msg = "> action? position:(%2d,%2d) character:%d hit_point:%d symbol:%c" % (int(d[1][1]), int(d[2][1]), int(d[3][1]), int(d[4][1]), d[5][1])
	print msg
	end_turn = False
	end_game = False
	while not end_turn:
		c = getch()
		if c == 'q':
			game.s.send("end\n")
			end_turn = True
			end_game = True
		elif c == 'w':
			game.s.send("north\n")
			end_turn = True
		elif c == 'd':
			game.s.send("east\n")
			end_turn = True
		elif c == 'a':
			game.s.send("west\n")
			end_turn = True
		elif c == 's':
			game.s.send("south\n")
			end_turn = True
		else:
			do_camera(game, c)
	move_to(0, game.win_height + 2)
	msg = " " * len(msg)
	print msg
	return end_game

def do_hide(game, d):
	assert(len(d) == 3)
	assert(len(d[1]) == 2 and d[1][0] == "x")
	assert(len(d[2]) == 2 and d[2][0] == "y")
	x = int(d[1][1])
	y = int(d[2][1])

	game.view[x][y] = (False, game.view[x][y][1])

	# Actualisation de l'affichage si necessaire.
	if x >= game.win_x and y >= game.win_y and x < game.win_x + game.win_width and y < game.win_y + game.win_height:
		update_view(game, x, y)

def do_print(game, d):
	assert(len(d) >= 4)
	assert(len(d[1]) == 2 and d[1][0] == "x")
	assert(len(d[2]) == 2 and d[2][0] == "y")
	x = int(d[1][1])
	y = int(d[2][1])

	change = False
	if d[3] == "terrain":
		assert(len(d) == 5 and d[4][0] == "type" and (True, d[4][1]) in tile)
		change = game.view[x][y] != (True, d[4][1])
		game.view[x][y] = (True, d[4][1])
	elif d[3] == "food":
		assert(len(d) == 6 and d[4][0] == "hit_point" and d[5][0] == "symbol" and (True, d[5][1]) in tile)
		change = game.view[x][y] != (True, d[5][1])
		game.view[x][y] = (True, d[5][1])
	elif d[3] == "ally" or d[3] == "ennemy":
		assert(len(d) == 7 and d[4][0] == "character_index" and d[5][0] == "hit_point" and d[6][0] == "symbol" and (True, d[6][1]) in tile)
		change = game.view[x][y] != (True, d[6][1])
		game.view[x][y] = (True, d[6][1])
	else:
		assert(False)

	# Actualisation de l'affichage si necessaire.
	if change and x >= game.win_x and y >= game.win_y and x < game.win_x + game.win_width and y < game.win_y + game.win_height:
		update_view(game, x, y)

def clear_screen():
	sys.stdout.write("\033c")

def echo_off():
	os.system("stty -echo")

def echo_on():
	os.system("stty echo")

init_stdin()
echo_off()
clear_screen()
end_game = False
end_message = "game finished"
while not end_game:
	ready, _, _ = select.select([game.s],[],[], 0.001)

	if game.s in ready:
		d = recv_line(s)
		#TODO
		print >> sys.stderr, d.strip()
		sys.stderr.flush()

		d = d.strip().split(" ")
		d = [x.split("=") for x in d]
		d = [x[0] if len(x) == 1 else x for x in d]

		if len(d) == 0:
			end_message = "invalid reception"
			end_game = True
		if len(d) == 1 and d[0] == "end":
			end_game = True
		elif d[0] == "print":
			do_print(game, d)
		elif d[0] == "hide":
			do_hide(game, d)
		elif d[0] == "action?":
			end_game = do_action(game, d)
	else:
		move_to(0, game.win_height + 2)
		c = getch()
		do_camera(game, c)
s.close()
echo_on()
restore_stdin()
