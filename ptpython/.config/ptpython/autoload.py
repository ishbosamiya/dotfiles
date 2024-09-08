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
