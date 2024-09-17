import numpy as np
import math

def lerp(x, y, t):
    """Linearly interpolate from `x` to `y` using `t`."""
    return x * (1.0 - t) + y * t

def csgo_angles_rad_to_forward_dir(angles_rad):
    """Convert the given CSGO space angles in radians to CSGO space
    forward direction."""
    yaw = angles_rad[1]
    pitch = angles_rad[0]
    sin_yaw =  np.sin(yaw)
    cos_yaw =  np.cos(yaw)
    sin_pitch =  np.sin(pitch)
    cos_pitch =  np.cos(pitch)
    return np.array([cos_pitch * cos_yaw, cos_pitch * sin_yaw, -sin_pitch])

def csgo_angles_deg_to_forward_dir(angles_deg):
    """Convert the given CSGO space angles in degrees to CSGO space
    forward direction."""
    return csgo_angles_rad_to_forward_dir(np.deg2rad(angles_deg))

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

def csgo_grenade_throw_angles_rad(eye_angles_rad):
    """Get the grenade throw angles in radians. Expects eye angles in radians."""
    throw_angles_rad = eye_angles_rad
    frac_pi_2 = math.pi / 2.0
    throw_angles_rad[0] -= np.radians(10.0) * (frac_pi_2 - abs(throw_angles_rad[0])) / frac_pi_2
    return throw_angles_rad

def csgo_grenade_start_position(eye_position,
                                eye_angles_deg,
                                start_throw_forward_by = 16.0):
    """Get the grenade start position. Expects eye angles in degrees."""
    throw_angles_rad = csgo_grenade_throw_angles_rad(np.deg2rad(eye_angles_deg))
    forward = csgo_angles_rad_to_forward_dir(throw_angles_rad)
    return eye_position + forward * start_throw_forward_by

def csgo_grenade_start_velocity(eye_angles_deg,
                                thrower_velocity = np.array([0.0, 0.0, 0.0]),
                                initial_grenade_throw_speed = 675.0,
                                thrower_velocity_multiplier = 1.25):
    """Get the grenade start velocity. Expects eye angles in degrees."""
    throw_angles_rad = csgo_grenade_throw_angles_rad(np.deg2rad(eye_angles_deg))
    forward = csgo_angles_rad_to_forward_dir(throw_angles_rad)
    return forward * initial_grenade_throw_speed + np.array(thrower_velocity) * thrower_velocity_multiplier

def csgo_grenade_start_position_and_velocity(eye_position,
                                             eye_angles_deg,
                                             thrower_velocity = np.array([0.0, 0.0, 0.0]),
                                             start_throw_forward_by = 16.0,
                                             initial_grenade_throw_speed = 675.0,
                                             thrower_velocity_multiplier = 1.25):
    """Get the grenade start position and velocity. Expects eye angles in degrees."""
    throw_position = csgo_grenade_start_position(eye_position, eye_angles_deg, start_throw_forward_by)
    throw_velocity = csgo_grenade_start_velocity(eye_angles_deg, thrower_velocity, initial_grenade_throw_speed, thrower_velocity_multiplier)
    return (throw_position, throw_velocity)
