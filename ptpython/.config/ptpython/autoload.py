import numpy as np
import math

def csgo_angles_deg_to_forward_dir(angles):
    """Convert the given CSGO space angles in degrees to CSGO space
    forward direction."""
    yaw = math.radians(angles[1]);
    pitch = math.radians(angles[0]);
    sin_yaw =  np.sin(yaw)
    cos_yaw =  np.cos(yaw)
    sin_pitch =  np.sin(pitch)
    cos_pitch =  np.cos(pitch)

    return np.array([cos_pitch * cos_yaw, cos_pitch * sin_yaw, -sin_pitch])

def csgo_forward_dir_to_angles_rad(forward):
    """Convert the given CSGO space forward direction to CSGO space
    angles in radians."""
    two_pi = math.pi * 2.0
    frac_pi_2 = math.pi / 2.0
    yaw = 0.0
    pitch = 0.0
    if forward[1] == 0.0 and forward[0] == 0.0:
        if forward[2] > 0.0:
            pitch = frac_pi_2 + frac_pi_2 + frac_pi_2
        else:
            pitch = frac_pi_2
    else:
        yaw = math.atan2(forward[1], forward[0])

        if yaw < 0.0:
            yaw = yaw + two_pi

        temp = math.sqrt(forward[0] * forward[0] + forward[1] * forward[1])

        pitch = -math.atan2(forward[2], temp)

        if pitch < 0.0:
            pitch = pitch + two_pi

    return np.array([pitch, yaw, 0.0])

def csgo_forward_dir_to_angles_deg(forward):
    """Convert the given CSGO space forward direction to CSGO space
    angles in degrees."""
    return np.rad2deg(csgo_forward_dir_to_angles_rad(forward))
