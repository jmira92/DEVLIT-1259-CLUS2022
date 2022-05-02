#!/usr/bin/env python3

"""NSO nano service example.

Demo script
(C) 2021 Tail-f Systems
Permission to use this code as a starting point hereby granted

See the README file for more information
"""
import subprocess
import os
import time
import sys
from datetime import datetime
import json
import requests

AUTH     = ('admin','admin')            # tuple of username, password
BASE_URL = 'http://localhost:8080/restconf'
HEADER = '\033[95m'
OKBLUE = '\033[94m'
OKGREEN = '\033[92m'
ENDC = '\033[0m'
BOLD = '\033[1m'

NVR=int(sys.argv[1])

def plan_state_status(nready, oper, status, state, name, notifications):
    """Check for a plan state and status """
    for notif_str in notifications:
        if len(notif_str) and "tailf-ncs:plan-state-change" in notif_str:
            notif = json.loads(notif_str)
            if notif["ietf-restconf:notification"]\
                ["tailf-ncs:plan-state-change"]\
                ["operation"] == "deleted":
                if oper == "deleted" and\
                    "/vrouter:vrouter[name=\'{}\']".format(name) in\
                    notif["ietf-restconf:notification"]\
                    ["tailf-ncs:plan-state-change"]\
                    ["service"] and state in\
                    notif["ietf-restconf:notification"]\
                    ["tailf-ncs:plan-state-change"]\
                    ["state"]:
                    nready +=1
                    print(f"\n{HEADER}##### " + name + f" deleted\n{ENDC}")
            elif oper != "deleted":
                if notif["ietf-restconf:notification"]\
                    ["tailf-ncs:plan-state-change"]\
                    ["status"] == status and\
                    "/vrouter:vrouter[name=\'{}\']".format(name) in\
                    notif["ietf-restconf:notification"]\
                    ["tailf-ncs:plan-state-change"]\
                    ["service"] and state in\
                    notif["ietf-restconf:notification"]\
                    ["tailf-ncs:plan-state-change"]\
                    ["state"]:
                    nready += 1
                    print(f"\n{OKGREEN}##### " + name + f" deployed\n{ENDC}")
    return nready

def service_cq_status(nready, status, name, notifications):
    """Check for a service commit queue status"""
    for notif_str in notifications:
        if len(notif_str) and "tailf-ncs:service-commit-queue-event"\
            in notif_str:
            notif = json.loads(notif_str)
            if notif["ietf-restconf:notification"]\
            ["tailf-ncs:service-commit-queue-event"]\
            ["status"] == status and\
            notif["ietf-restconf:notification"]\
            ["tailf-ncs:service-commit-queue-event"]["trace-id"] == name:
                nready += 1
    return nready

session = requests.Session()
session.auth = AUTH
headers = {'Content-Type': 'application/yang-data+json'}
headers_patch = {'Content-Type': 'application/yang-patch+json'}

print(f"\n{OKGREEN}##### Reset and setup the example\n{ENDC}")
subprocess.run(['make', 'stop', 'clean', 'all', 'start'], check=True,
               encoding='utf-8')

print(f"\n{OKBLUE}##### Deploy and configure"\
      " {} vrouter through the".format(NVR) +
      " vrouter nano service, but immediately delete the service during init" +
      f" so that the nano service backtrack\n{ENDC}")

EDIT_STR=""
for i in range(0,NVR):
    EDIT_STR += '''
      {{
        "edit-id" : "edit{0}",
        "operation" : "create",
        "target" : "/vrouter:vrouter=vr{0}",
        "value" : {{
          "vrouter:vrouter": [
            {{
              "name": "vr{0}",
              "iface": "eth{1}",
              "unit": {0},
              "vid": {0}
            }}
          ]
        }}
      }},'''.format(i+1,i)

PATCH_STR = '''{{
  "ietf-yang-patch:yang-patch" : {{
    "patch-id" : "add-vrouter-1",
    "edit" : [{}
    ]
  }}
}}'''.format(EDIT_STR[:-1])

PATH = '/data?dry-run'
print(f"{BOLD}PATCH " + BASE_URL + PATH + f"{ENDC}")
print(f"{HEADER}" + PATCH_STR + f"{ENDC}")
r = session.patch(BASE_URL + PATH, data=PATCH_STR,
headers=headers_patch)
print(r.text)

PATH = '/data'
print(f"{BOLD}PATCH " + BASE_URL + PATH + f"{ENDC}")
print(f"{HEADER}" + PATCH_STR + f"{ENDC}")
r = session.patch(BASE_URL + PATH, data=PATCH_STR, headers=headers_patch)
print(r.text)

EDIT_STR=""
for i in range(0,NVR):
    EDIT_STR += '''
      {{
        "edit-id" : "edit{0}",
        "operation" : "delete",
        "target" : "/vrouter:vrouter=vr{0}"
      }},'''.format(i+1)

PATCH_STR = '''{{
  "ietf-yang-patch:yang-patch" : {{
    "patch-id" : "delete-vrouter-1",
    "edit" : [{}
    ]
  }}
}}'''.format(EDIT_STR[:-1])

PATH = '/data?dry-run'
print(f"{BOLD}PATCH " + BASE_URL + PATH + f"{ENDC}")
print(f"{HEADER}" + PATCH_STR + f"{ENDC}")
r = session.patch(BASE_URL + PATH, data=PATCH_STR,
headers=headers_patch)
print(r.text)

dt_string = datetime.utcnow().isoformat()

PATH = '/data'
print(f"{BOLD}PATCH " + BASE_URL + PATH + f"{ENDC}")
print(f"{HEADER}" + PATCH_STR + f"{ENDC}")
r = session.patch(BASE_URL + PATH, data=PATCH_STR, headers=headers_patch)
print(r.text)

PATH = '/data/tailf-ncs:zombies'
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

PATH = '/data/tailf-ncs:side-effect-queue'
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

headers_stream = {'Content-Type': 'text/event-stream'}
PATH = '/streams/service-state-changes/json?start-time=' + dt_string
print(f"\n{HEADER}##### Waiting for plan notifications for all deleted "\
      f"nano service components to have reached the init state...{ENDC}")
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
with session.get(BASE_URL + PATH, headers=headers_stream, stream=True) as r:
    NREADY = 0
    i = 0
    for notifs_str in r.iter_content(chunk_size=None, decode_unicode=True):
        notifs_str = notifs_str.replace('data: ', '')
        print(f"{HEADER}" + notifs_str + f"{ENDC}")
        notifs = notifs_str.split("\n\n")
        for i in range(0,NVR):
            NREADY = plan_state_status(NREADY, "deleted", "", "tailf-ncs:init",
                                        "vr{}".format(i+1), notifs)
        if NREADY == NVR:
            break

print(f"{OKBLUE}##### Deploy and configure" +\
      " {0} vrouter through".format(NVR) +\
      f" the vrouter nano service{ENDC}")

EDIT_STR=""
for i in range(NVR,NVR*2):
    EDIT_STR += '''
      {{
        "edit-id" : "edit{0}",
        "operation" : "create",
        "target" : "/vrouter:vrouter=vr{0}",
        "value" : {{
          "vrouter:vrouter": [
            {{
              "name": "vr{0}",
              "iface": "eth{1}",
              "unit": {0},
              "vid": {0}
            }}
          ]
        }}
      }},'''.format(i+1,i)

PATCH_STR = '''{{
  "ietf-yang-patch:yang-patch" : {{
    "patch-id" : "add-vrouter-2",
    "edit" : [{}
    ]
  }}
}}'''.format(EDIT_STR[:-1])

PATH = '/data?dry-run'
print(f"{BOLD}PATCH " + BASE_URL + PATH + f"{ENDC}")
print(f"{HEADER}" + PATCH_STR + f"{ENDC}")
r = session.patch(BASE_URL + PATH, data=PATCH_STR,
headers=headers_patch)
print(r.text)

dt_string = datetime.utcnow().isoformat()

PATH = '/data'
print(f"{BOLD}PATCH " + BASE_URL + PATH + f"{ENDC}")
print(f"{HEADER}" + PATCH_STR + f"{ENDC}")
r = session.patch(BASE_URL + PATH, data=PATCH_STR, headers=headers_patch)
print(r.text)

PATH = '/data/tailf-ncs:side-effect-queue'
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + '/data/tailf-ncs:side-effect-queue', headers=headers)
print(r.text)

headers_stream = {'Content-Type': 'text/event-stream'}
PATH = '/streams/service-state-changes/json?start-time=' + dt_string
print(f"\n{HEADER}##### Waiting for plan notifications for all nano service"\
      f" components to have reached the ready state...{ENDC}")
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
with session.get(BASE_URL + PATH, headers=headers_stream, stream=True) as r:
    NREADY = 0
    i = 0
    for notifs_str in r.iter_content(chunk_size=None, decode_unicode=True):
        notifs_str = notifs_str.replace('data: ', '')
        print(f"{HEADER}" + notifs_str + f"{ENDC}")
        notifs = notifs_str.split("\n\n")
        for i in range(NVR,NVR*2):
            NREADY = plan_state_status(NREADY, "", "reached", "tailf-ncs:ready",
                                        "vr{}".format(i+1), notifs)
        if NREADY == NVR:
            break

PATH = '/data/vrouter:vrouter'
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

PATH = '/data/tailf-ncs:devices/device?content=config'
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

print(f"{OKBLUE}##### Make some configuration changes to the vrouter service"\
      f"{ENDC}")
PATCH_STR = '''{{
  "vrouter:vrouter": [
    {{
      "name" : "vr{}",
      "iface" : "eth99",
      "unit" : "99",
      "vid" : "99"
    }}
  ]
}}'''.format(NVR+1)
PATH = '/data/vrouter:vrouter=vr{}'.format(NVR+1)
print(f"{BOLD}PATCH " + BASE_URL + PATH + f"{ENDC}")
print(f"{HEADER}" + PATCH_STR + f"{ENDC}")
r = session.patch(BASE_URL + PATH, data=PATCH_STR, headers=headers)
print("Status code: {}\n".format(r.status_code))

PATH = '/data/tailf-ncs:devices/device=vr{}/config?content=config'.format(NVR+1)
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

print(f"{OKBLUE}##### Delete vrouter service to backtrack{ENDC}")

PATH = '/data/vrouter:vrouter'
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

EDIT_STR=""
for i in range(NVR,NVR*2):
    EDIT_STR += '''
      {{
        "edit-id" : "edit{0}",
        "operation" : "delete",
        "target" : "/vrouter:vrouter=vr{0}"
      }},'''.format(i+1)

PATCH_STR = '''{{
  "ietf-yang-patch:yang-patch" : {{
    "patch-id" : "delete-vrouter-2",
    "edit" : [{}
    ]
  }}
}}'''.format(EDIT_STR[:-1])

PATH = '/data?dry-run'
print(f"{BOLD}PATCH " + BASE_URL + PATH + f"{ENDC}")
print(f"{HEADER}" + PATCH_STR + f"{ENDC}")
r = session.patch(BASE_URL + PATH, data=PATCH_STR, headers=headers_patch)
print(r.text)

dt_string = datetime.utcnow().isoformat()

PATH = '/data'
print(f"{BOLD}PATCH " + BASE_URL + PATH + f"{ENDC}")
print(f"{HEADER}" + PATCH_STR + f"{ENDC}")
r = session.patch(BASE_URL + PATH, data=PATCH_STR, headers=headers_patch)
print(r.text)

PATH = '/data/tailf-ncs:zombies'
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

PATH = '/data/tailf-ncs:side-effect-queue'
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

headers_stream = {'Content-Type': 'text/event-stream'}
PATH = '/streams/service-state-changes/json?start-time=' + dt_string

print(f"\n{HEADER}##### Waiting for plan notifications for all deleted "\
      f"nano service components to have reached the init state...{ENDC}")
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
with session.get(BASE_URL + PATH, headers=headers_stream, stream=True) as r:
    NREADY = 0
    i = 0
    for notifs_str in r.iter_content(chunk_size=None, decode_unicode=True):
        notifs_str = notifs_str.replace('data: ', '')
        print(f"{HEADER}" + notifs_str + f"{ENDC}")
        notifs = notifs_str.split("\n\n")
        for i in range(NVR,NVR*2):
            NREADY = plan_state_status(NREADY, "deleted", "", "tailf-ncs:init",
                                        "vr{}".format(i+1), notifs)
        if NREADY == NVR:
            break

print(f"{OKBLUE}##### Deploy and configure a vrouter through the vrouter nano"\
      f" service, but set the admin state on the netsim device "\
      f"to \"locked\"{ENDC}")
PATCH_STR = '''{{
  "vrouter:vrouter": [
    {{
      "name" : "vr{0}",
      "iface" : "eth{1}",
      "unit" : "{0}",
      "vid" : "{0}"
    }}
  ]
}}'''.format(NVR*2+1,NVR*2)

PATH = '/data/vrouter:vrouter?dry-run'
print(f"{BOLD}PATCH " + BASE_URL + PATH + f"{ENDC}")
print(f"{HEADER}" + PATCH_STR + f"{ENDC}")
r = session.patch(BASE_URL + PATH, data=PATCH_STR, headers=headers)
print(r.text)

dt_string = datetime.utcnow().isoformat()

PATH = '/data/vrouter:vrouter?commit-queue=sync&'\
       'commit-queue-error-option=rollback-on-error&trace-id'\
       '=vr{}'.format(NVR*2+1)
print(f"{BOLD}PATCH " + BASE_URL + PATH + f"{ENDC}")
print(f"{HEADER}" + PATCH_STR + f"{ENDC}")
r = session.patch(BASE_URL + PATH, data=PATCH_STR, headers=headers)
print("Status code: {}\n".format(r.status_code))

NETSIM_PORT = ""
while True:
    try:
        r = subprocess.run(['ncs-netsim', 'get-port', 'vr{}'.format(NVR*2+1)],
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                       check=True, encoding='utf-8')
        NETSIM_PORT = r.stdout
    except subprocess.CalledProcessError:
        print(f"{HEADER}##### Waiting for the netsim device to be "\
              f"created to get the port number...{ENDC}")
        time.sleep(1)
        continue
    break

while True:
    try:
        subprocess.run([
            '{}/netsim/confd/bin/confd_cmd'.format(os.getenv('NCS_DIR')),
            '-p', NETSIM_PORT, '-c', 'wait-start 1'], stderr=subprocess.PIPE,
            check=True, encoding='utf-8')
    except subprocess.CalledProcessError:
        print(f"{HEADER}##### Waiting for the netsim device to start...{ENDC}")
        time.sleep(.5)
        continue
    break

print(f"{OKBLUE}##### Set admin state to \"locked\" on the netsim device to "\
      f"make the next transaction fail{ENDC}")
subprocess.run(['{}/netsim/confd/bin/confd_cmd'.format(os.getenv('NCS_DIR')),
                '-o', '-p', NETSIM_PORT, '-c',
                'mset "/r:sys/state/admin-state" "locked"'], check=True,
               encoding='utf-8')

headers_stream = {'Content-Type': 'text/event-stream'}
PATH = '/streams/service-state-changes/json?start-time=' + dt_string
print(f"\n{HEADER}##### Waiting for the service commit queue failed "\
      f"event notification and thereby the nano service plan fail due to "\
      f"the transaction being aborted by the device...{ENDC}")
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
with session.get(BASE_URL + PATH, headers=headers_stream, stream=True) as r:
    NUM = 0
    for notifs_str in r.iter_content(chunk_size=None, decode_unicode=True):
        notifs_str = notifs_str.replace('data: ', '')
        print(f"{HEADER}" + notifs_str + f"{ENDC}")
        notifs = notifs_str.split("\n\n")
        if service_cq_status(NUM, "failed", "vr{}".format(NVR*2+1), notifs):
            break

PATH = '/data/vrouter:vrouter=vr{}/plan/failed'.format(NVR*2+1)
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print("{}\n".format(list(r.json().keys())[0]))

PATH = '/data/vrouter:vrouter=vr{}/plan/error-info/message'.format(NVR*2+1)
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

PATH = '/data/vrouter:vrouter=vr{}/plan/component'.format(NVR*2+1)
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

print(f"{OKBLUE}##### Set admin state to \"unlocked\" on the netsim device to "\
      f"make the next transaction succeed{ENDC}")
subprocess.run(['{}/netsim/confd/bin/confd_cmd'.format(os.getenv('NCS_DIR')),
                '-o', '-p', NETSIM_PORT, '-c',
                'mset "/r:sys/state/admin-state" "unlocked"'], check=True,
               encoding='utf-8')

print(f"{OKBLUE}##### Redeploy{ENDC}")

dt_string = datetime.utcnow().isoformat()

PATH = '/data/vrouter:vrouter=vr{}/re-deploy'.format(NVR*2+1)
print(f"{BOLD}POST " + BASE_URL + PATH + f"{ENDC}")
r = session.post(BASE_URL + PATH, headers=headers)
print("Status code: {}\n".format(r.status_code))

headers_stream = {'Content-Type': 'text/event-stream'}
PATH = '/streams/service-state-changes/json?start-time=' + dt_string
print(f"\n{HEADER}##### Waiting for a plan notification for the nano service"\
      f" components to have reached the ready state...{ENDC}")
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
with session.get(BASE_URL + PATH, headers=headers_stream, stream=True) as r:
    NUM = 0
    for notifs_str in r.iter_content(chunk_size=None, decode_unicode=True):
        notifs_str = notifs_str.replace('data: ', '')
        print(f"{HEADER}" + notifs_str + f"{ENDC}")
        notifs = notifs_str.split("\n\n")
        if plan_state_status(NUM, "", "reached", "tailf-ncs:ready",
                                "vr{}".format(NVR*2+1), notifs):
            break

PATH = '/data/vrouter:vrouter=vr{}/plan/component'.format(NVR*2+1)
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

PATH = '/data/vrouter:vrouter=' +\
       'vr{0}/plan/component=vrouter,vr{0}-day0/'.format(NVR*2+1) +\
       'state=onboarded/get-modifications'
print(f"{BOLD}POST " + BASE_URL + PATH + f"{ENDC}")
r = session.post(BASE_URL + PATH, headers=headers)
print(r.json()["vrouter:output"]["cli"]["local-node"]["data"])

PATH = '/data/vrouter:vrouter=' +\
       'vr{0}/plan/component=vrouter-day1,vr{0}-day1/'.format(NVR*2+1) +\
       'state=configured/get-modifications'
print(f"{BOLD}POST " + BASE_URL + PATH + f"{ENDC}")
r = session.post(BASE_URL + PATH, headers=headers)
print(r.json()["vrouter:output"]["cli"]["local-node"]["data"])

PATH = '/data/tailf-ncs:devices/device='\
       'vr{}/config?content=config'.format(NVR*2+1)
print(f"{BOLD}GET " + BASE_URL + PATH + f"{ENDC}")
r = session.get(BASE_URL + PATH, headers=headers)
print(r.text)

print(f"{OKGREEN}##### Done!{ENDC}")
