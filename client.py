#!/usr/bin/env python

import socket
import argparse
import json
import time
import struct

HOST = "localhost"
PORT = 15556

parser = argparse.ArgumentParser()
parser.add_argument("-s", "--sentence", help="sentence to structure")
args = parser.parse_args()
sentence = args.sentence

def send_msg(sock, msg):
  msg = struct.pack('>I', len(msg)) + msg
  sock.sendall(msg)

def recv_msg(sock):
  raw_msglen = recvall(sock, 4)
  if not raw_msglen:
      return None
  msglen = struct.unpack('>I', raw_msglen)[0]
  return recvall(sock, msglen)

def recvall(sock, n):
  data = ''
  while len(data) < n:
    packet = sock.recv(n - len(data))
    if not packet:
      return None
    data += packet
  return data

socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
socket.connect((HOST, PORT))

send_msg(socket, sentence)
response = recv_msg(socket)

print response
socket.close()
