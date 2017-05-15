#!/usr/bin/env python

import argparse
from util import *

HOST = "localhost"
PORT = 15556

parser = argparse.ArgumentParser()
parser.add_argument("-s", "--sentence", help="sentence to structure")
args = parser.parse_args()
sentence = args.sentence

socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
socket.connect((HOST, PORT))

send_msg(socket, sentence)
response = recv_msg(socket)

print response
socket.close()
