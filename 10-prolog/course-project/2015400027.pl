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
  % Get all the courses that students take in a flattened list and call it L2.
  findall(X, student(_, X), L1), flatten(L1, L2),
  % Get rid of the duplicate courses in L2.
  list_to_set(L2, CourseList).

% all_rooms/1 is true iff RoomList contains all the rooms.
all_rooms(RoomList) :-
  findall(X, room_capacity(X, _), RoomList).

% common_students/3 is true iff StudentCount is the common student count of CourseID1 and CourseID2.
common_students(CourseID1, CourseID2, StudentCount) :-
  % Get the students who takes CourseID1 and CourseID2.
  students(CourseID1, Students1), students(CourseID2, Students2),
  % Get the common students in lists Students1 and Students2.
  intersection(Students1, Students2, CommonStudents),
  % Get the length of the common students.
  length(CommonStudents, StudentCount).

% comb/3 generates N-element combinations of a list.
% This rule is taken from http://kti.ms.mff.cuni.cz/~bartak/prolog/combinatorics.html
comb(0, _, []).
comb(N, [X|T], [X|Comb]) :-
  N > 0, N1 is N - 1, comb(N1, T, Comb).
comb(N, [_|T], Comb) :-
  N > 0, comb(N, T, Comb).

% slot_error/3 is true iff Error is the slot error of given final plan parts.
% If final plan parts have the same slot, slot error is the common student count of courses.
% Otherwise, slot error is 0.
slot_error([CourseID1, _, Slot1], [CourseID2, _, Slot1], Error) :-
  % Use a cut to not retry evaluating with the base case below.
  % Base case should only be evaluated if slots of the final plan parts are different.
  common_students(CourseID1, CourseID2, Error), !.
slot_error(_, _, 0).

% slot_err_acc/3 is an accumulator for a fold in errors_for_plan/2.
% It is true iff Error is the sum of PreviousError and slot error of
% FinalPlanPart1 and FinalPlanPart2 (which is calculated by slot_error/3).
slot_err_acc([FinalPlanPart1, FinalPlanPart2], PreviousError, Error) :-
  slot_error(FinalPlanPart1, FinalPlanPart2, SlotError),
  Error is PreviousError + SlotError.

% room_slot_conflict/3 is true iff Error is the room & slot conflict of given final plan parts.
% Conflict is 0 if final plan parts have different room or slot.
% Conflict is the sum of student counts if final plan parts have the same room and slot.
room_slot_conflict([CourseID1, RoomID1, Slot1], [CourseID2, RoomID1, Slot1], Error) :-
  student_count(CourseID1, StudentCount1), student_count(CourseID2, StudentCount2),
  % Use a cut to not retry evaluating with the base case below.
  % Base case should only be evaluated if slots and rooms of the final plan parts are different.
  Error is StudentCount1 + StudentCount2, !.
room_slot_conflict(_, _, 0).

% room_slot_conflict_acc/3 is an accumulator for a fold in conflicts_for_plan/2.
% It is true iff Error is the sum of PreviousError and room & slot conflict of
% FinalPlanPart1 and FinalPlanPart2 (which is calculated by room_slot_conflict/3).
room_slot_conflict_acc([FinalPlanPart1, FinalPlanPart2], PreviousError, Error) :-
  room_slot_conflict(FinalPlanPart1, FinalPlanPart2, RoomSlotConflict),
  Error is PreviousError + RoomSlotConflict.

% capacity_error_acc/3 is an accumulator for a fold in errors_for_plan/2.
% It is true iff Error is the sum of PreviousError and capacity overflow of RoomID
% assuming we assign the final exam of CourseID to RoomID.
capacity_error_acc([CourseID, RoomID, _], PreviousError, Error) :-
  room_capacity(RoomID, RoomCapacity),
  student_count(CourseID, NumberOfAttendees),
  % Use a cut here to not go to the base case below if NumberOfAttendees is bigger than RoomCapacity.
  NumberOfAttendees > RoomCapacity, !,
  % NumberOfAttendees - RoomCapacity is the capacity error of the final plan part.
  Error is NumberOfAttendees - RoomCapacity + PreviousError.
capacity_error_acc(_, PreviousError, PreviousError).

% errors_for_plan/2 is true iff ErrorCount is the total error count of Plan.
% Folds over the 2 element combinations of Plan, uses slot_err_acc/3 and
% capacity_error_acc/3 accumulators to get the count of all errors of Plan.
errors_for_plan([], 0) :- !.
errors_for_plan(Plan, ErrorCount) :-
  % Find all 2-element combinations of final plan parts.
  findall(X, comb(2, Plan, X), TwoCombinations),
  % Fold over the final plan parts to find all errors, type by type.
  foldl(slot_err_acc, TwoCombinations, 0, SlotErrorCount),
  foldl(capacity_error_acc, Plan, SlotErrorCount, ErrorCount).

% errors_for_plan/2 is true iff ErrorCount is the total error count of Plan.
% Folds over the 2 element combinations of Plan, uses slot_err_acc/3,
% capacity_error_acc/3 and slot_room_conflict/3 accumulators to get the count of
% all errors of Plan.
conflicts_for_plan([], 0) :- !.
conflicts_for_plan(Plan, ErrorCount) :-
  % Find all 2-element combinations of final plan parts.
  findall(X, comb(2, Plan, X), TwoCombinations),
  % Fold over the final plan parts to find all errors, type by type.
  foldl(slot_err_acc, TwoCombinations, 0, SlotErrorCount),
  foldl(room_slot_conflict_acc, TwoCombinations, SlotErrorCount, RoomSlotErrorCount),
  foldl(capacity_error_acc, Plan, RoomSlotErrorCount, ErrorCount).

% final_plan_generate_acc/3 is the accumulator for the fold in final_plan/1.
% It is true if Plan is Course added to PreviousPlan with a proper room and slot
% so that it has 0 error. It uses errors_for_plan/2 to calculate the error count.
final_plan_generate_acc(Course, PreviousPlan, Plan) :-
  % Get all the slots and rooms, and pick one element from both.
  available_slots(Slots), all_rooms(Rooms), member(Slot, Slots), member(Room, Rooms),
  % Construct a part of the final plan and call it FinalPlanPart.
  FinalPlanPart = [Course, Room, Slot],
  % Append the final plan part to PreviousPlan.
  append([FinalPlanPart], PreviousPlan, Plan),
  % New plan, which is FinalPlanPart appended to PreviousPlan, should have 0 error.
  conflicts_for_plan(Plan, 0).

% final_plan/1 is true iff Plans is a course plan in the format
% [[CourseID, RoomID, Slot], [CourseID2, RoomID2, Slot2], ...] with 0 errors.
% Errors are calculated in the accumulator final_plan_generate_acc/3.
final_plan(Plans) :-
  all_courses(Courses),
  % Fold over the courses and find an appropriate room and slot for each one.
  foldl(final_plan_generate_acc, Courses, [], Plans).

% clear_knowledge_base/0 retracts student/2, available_slots/1 and room_capacity/2,
% and writes how many facts it has retracted.
clear_knowledge_base :-
  % Get the length of the student facts.
  all_students(Students), length(Students, StudentCount),
  % Get the length of the slot facts.
  findall(X, available_slots(X), Slots), length(Slots, SlotsCount),
  % Get the length of the room facts.
  all_rooms(Rooms), length(Rooms, RoomCount),
  % Retract the students and log the operation with the number of retracted student facts.
  retractall(student(_,_)), write('student/2: '), write(StudentCount), write('\n'),
  % Retract the slots and log the operation with the number of retracted slot facts.
  retractall(available_slots(_)), write('available_slots/1: '), write(SlotsCount), write('\n'),
  % Retract the rooms and log the operation with the number of retracted room facts.
  retractall(room_capacity(_,_)), write('room_capacity/2: '), write(RoomCount), write('\n').
