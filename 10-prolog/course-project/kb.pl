:- dynamic student/2.
student(1, ['math101']).
student(2, ['cmpe150']).

:- dynamic available_slots/1.
available_slots(['w-1']).

:- dynamic room_capacity/2.
room_capacity('nh101', 5).
room_capacity('nh102', 5).
