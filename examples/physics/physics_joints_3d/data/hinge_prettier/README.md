Room + doors + windows done in Blender.

With the help of Archimesh add-on.

To export:

- Select "Door" (child of "Door Frame") recursively ("Select hierarchy" in Outliner) and export to `joint_hinge_door.glb`

- Invert selection (Ctrl + I) and export to `joint_hinge_rest.glb`

Note: the door and the rest collide at the start a bit.
To avoid having physics bodies colliding at start, we set Scale = 0.8 on door box colliders.
