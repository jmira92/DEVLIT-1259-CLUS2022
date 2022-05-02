# -*- mode: python; python-indent: 4 -*-
#
# Copyright 2015 Cisco Inc.
#
import _ncs
import json
import socket
import struct
import logging

#
# C O N T R O L   S O C K E T
#


class ControlSocket(object):
    def __init__(self, vmid):
        self._logger = logging.getLogger(vmid)
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        # client_id needs to be in sync with this entry in confd.hrl:
        #   -define(IA_CLIENT_NCS_PY,    21).
        client_id = 21
        _ncs.internal_connect(client_id, self.sock, '127.0.0.1', _ncs.PORT)

    def send(self, data):
        if not isinstance(data, bytes):
            data = bytes(data, 'utf-8')
        msglen = len(data)
        totalsent = 0
        while totalsent < msglen:
            sent = self.sock.send(data[totalsent:])
            if sent == 0:
                raise RuntimeError("socket connection broken")
            totalsent = totalsent + sent

    def send_json(self, msg):
        data = json.dumps(msg)
        # First send the 4-byte packet length
        self.dbg('Send Packet length = ' + str(len(data)))
        x = struct.pack('!I', len(data))
        self.send(x)
        # Then send the Json packet
        self.dbg('Send Packet data = ' + data)
        self.send(data)

    def recv(self, msglen):
        chunks = bytearray()
        bytes_recd = 0
        while bytes_recd < msglen:
            chunk = self.sock.recv(min(msglen - bytes_recd, 2048))
            if not chunk:
                raise RuntimeError("socket connection broken")
            chunks.extend(chunk)
            bytes_recd = bytes_recd + len(chunk)
        return bytes(chunks)

    def recv_json(self):
        # First read the 4-byte packet length
        x = struct.unpack('!I', self.recv(4))
        plen = x[0]
        self.dbg('Recv Packet plen = ' + str(plen))

        # Now read the Json packet
        data = self.recv(plen)
        data = data.decode()
        self.dbg('DATA: ' + data)
        return dict(json.loads(data))

    def close(self):
        self.sock.close()

    def dbg(self, line):
        self._logger.debug(line)

    def info(self, line):
        self._logger.info(line)


class SafeControlSocket(object):
    def __init__(self, vmid):
        self.vmid = vmid

    def __enter__(self):
        self.s = ControlSocket(self.vmid)
        return self.s

    def __exit__(self, type, value, traceback):
        self.s.close()
        return True    # suppress any exceptions
