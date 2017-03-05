% students/2 is true iff Students contains all the students who takes the course CourseID.
students(CourseID, Students) :-
  findall(X, (student(X, Courses), member(CourseID, Courses)), Students).

% student_count/2 is true iff StudentCount is the count of students who takes the course CourseID.
student_count(CourseID, StudentCount) :-
  students(CourseID, L), length(L, StudentCount).

% all_students/1 is true iff StudentList contains all the students.
all_students(StudentList) :-
  findall(X, student(X, _), StudentList).

% all_courses/1 is true iff CourseList contains all the courses.
all_courses(CourseList) :-
  findall(X, student(_, X), L1), flatten(L1, L2),
  list_to_set(L2, CourseList).

% all_rooms/1 is true iff RoomList contains all the rooms.
all_rooms(RoomList) :-
  findall(X, room_capacity(X, _), RoomList).

% common_students/3 is true iff StudentCount is the common student count of
% CourseID1 and CourseID2.
common_students(CourseID1, CourseID2, StudentCount) :-
  students(CourseID1, Students1),
  students(CourseID2, Students2),
  intersection(Students1, Students2, CommonStudents),
  length(CommonStudents, StudentCount).

% comb/3 generates N-element combinations of a list.
% This rule is taken from http://kti.ms.mff.cuni.cz/~bartak/prolog/combinatorics.html
comb(0, _, []).
comb(N, [X|T], [X|Comb]) :-
  N > 0, N1 is N - 1, comb(N1, T, Comb).
comb(N, [_|T], Comb) :-
  N > 0, comb(N, T, Comb).

% slot_error/3 is true iff Error is the slot error of given 2 course final plans.
%
% Calculation of slot error:
% If 2 course final plans have the same slot, slot error is the common student
% count of those courses. Otherwise, it is 0.
slot_error([CourseID1, _, Slot1], [CourseID2, _, Slot2], Error) :-
  (  Slot1 == Slot2
  -> (common_students(CourseID1, CourseID2, CommonStudentCount), Error is CommonStudentCount)
  ;  Error is 0
  ), !.

% slot_err_acc/3 is an accumulator for a fold in errors_for_plan/2.
% It is true iff Error is the sum of PreviousError and slot error of
% CourseFinalPlan1 and CourseFinalPlan2 (which is calculated by slot_error/3).
slot_err_acc([CourseFinalPlan1, CourseFinalPlan2], PreviousError, Error) :-
  slot_error(CourseFinalPlan1, CourseFinalPlan2, SlotError),
  Error is PreviousError + SlotError, !.

% capacity_error_acc/3 is an accumulator for a fold in errors_for_plan/2.
% It is true iff Error is the sum of PreviousError and capacity overflow of RoomID
% assuming we assign the final of CourseID to RoomID.
capacity_error_acc([CourseID, RoomID, _], PreviousError, Error) :-
  room_capacity(RoomID, RoomCapacity),
  student_count(CourseID, NumberOfAttendee),
  (  NumberOfAttendee > RoomCapacity
  -> Error is NumberOfAttendee - RoomCapacity + PreviousError
  ;  Error is PreviousError
  ), !.

% errors_for_plan/2 is true iff ErrorCount is the error count of Plan.
errors_for_plan([], 0) :- !.
errors_for_plan([_], 0) :- !.
errors_for_plan(Plan, ErrorCount) :-
  findall(X, comb(2, Plan, X), TwoCombinations),
  foldl(slot_err_acc, TwoCombinations, 0, SlotErrorCount),
  foldl(capacity_error_acc, Plan, SlotErrorCount, ErrorCount),
  !.

% final_plan_generate_acc/3 is the accumulator for the fold in final_plan/1.
% It is true if Plan is Course added to PreviousPlan with a proper room and slot
% so that it has 0 error.
final_plan_generate_acc(Course, PreviousPlan, Plan) :-
  available_slots(Slots), all_rooms(Rooms), !,
  member(Slot, Slots), member(Room, Rooms),
  PlanPart = [Course, Room, Slot],
  append([PlanPart], PreviousPlan, Plan),
  errors_for_plan(Plan, 0).

% final_plan/1 is true iff Plans is a course plan in the format
% [[CourseID, RoomID, Slot] | ...] with 0 errors.
final_plan(Plans) :-
  all_courses(Courses),
  foldl(final_plan_generate_acc, Courses, [], Plans).

% clear_knowledge_base/0 retracts student/2, available_slots/1 and room_capacity/2,
% and writes how many facts it has retracted.
clear_knowledge_base :-
  all_students(Students), length(Students, StudentCount),
  findall(X, available_slots(X), SlotDeclarations), length(SlotDeclarations, SlotDeclarationCount),
  all_rooms(Rooms), length(Rooms, RoomCount),
  retractall(student(_,_)), write('student/2: '), write(StudentCount), write('\n'),
  retractall(available_slots(_)), write('available_slots/1: '), write(SlotDeclarationCount), write('\n'),
  retractall(room_capacity(_,_)), write('room_capacity/2: '), write(RoomCount), write('\n').
