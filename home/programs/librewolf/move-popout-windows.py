"""
Move librewolf popout windows to specific niri workspaces

Adapted from: https://github.com/YaLTeR/niri/discussions/1599
"""

import json
import os
import re
from socket import AF_UNIX, SHUT_WR, socket

app_id = 'librewolf'

title_matcher = re.compile(
    '(https://www.youtube.com'
    '|https://music.youtube.com'
    '|YouTube Music)|'

    '(https://calendar.google.com'
    '|https://calendar.proton.me'
    '|http://habitica.jakira.space)|'

    '(https://mail.google.com'
    '|https://mail.proton.me'
    '|https://outlook.office.com'
    '|https://app.cinny.in'
    '|https://app.element.io'
    '|https://chat.jakira.space)'
)

workspaces = ["1-browsing", "3-planning", "4-communicating"]


def send(request):
    with socket(AF_UNIX) as niri_socket:
        niri_socket.connect(os.environ['NIRI_SOCKET'])
        file = niri_socket.makefile('rw')
        _ = file.write(json.dumps(request))
        file.flush()


def workspace_reference(workspace):
    if workspace is int:
        return {'Index': workspace}
    else:
        return {'Name': workspace}


def match_window(window):
    if window['app_id'] == app_id:
        match = title_matcher.match(window['title'])
        if match is None:
            return

        reference = None
        for i, workspace in enumerate(workspaces):
            if match.group(i + 1) is not None:
                reference = workspace_reference(workspace)

        if reference := reference:
            send({'Action': {'MoveWindowToWorkspace': {
                'window_id': window['id'],
                'reference': reference,
                'focus': False
            }}})


niri_socket = socket(AF_UNIX)
niri_socket.connect(os.environ['NIRI_SOCKET'])
file = niri_socket.makefile('rw')

_ = file.write('"EventStream"')
file.flush()
niri_socket.shutdown(SHUT_WR)

for line in file:
    event = json.loads(line)
    if changed := event.get('WindowsChanged'):
        for window in changed['windows']:
            match_window(window)
    elif changed := event.get('WindowOpenedOrChanged'):
        match_window(changed['window'])
