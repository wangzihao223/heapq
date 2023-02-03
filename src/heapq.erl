-module(heapq).

-export([new/0]).
-export([from_array/1]).
% -export([push/2]).
-export([test_from_array/0]).



new() -> array:new().

% ceate heap from array
from_array(Array)->
    Length = array:size(Array),
    {NewArray, _} = loop_find_small(Array, 0, Length),
    NewArray.

% compare a and b
small_one({LeftIndex, LeftValue}, {RightIndex, RightValue}) ->
    % get priority
    {LeftPriority, _} = LeftValue,
    {RightPriority, _} = RightValue,
    if LeftPriority < RightPriority ->
        {LeftIndex, LeftValue};
        true ->
            {RightIndex, RightValue}
    end.

% exchange a and b
exchange_node({AIndex, AValue}, {BIndex, BValue}, Array) ->
    Array2 = array:set(AIndex, BValue, Array),
    Array3 = array:set(BIndex, AValue, Array2),
    Array3.

% find small 
find_small(Array , ParentIndex, Length) ->
    % three condition
    LeftTreeIndex = ParentIndex * 2 + 1,
    RightTreeIndex = ParentIndex * 2 + 2,

    % get parent value
    ParentValue = array:get(ParentIndex, Array),
    {ParentPriority, _} = ParentValue,

    % first: not exist child
    if LeftTreeIndex >= Length ->
        % return parent index and Array
        {ParentIndex, Array};
    % second: both exist child
        RightTreeIndex < Length ->
            % find left child tree and right child tree
            % small value
            {LeftIndex, Array1} = find_small(Array, LeftTreeIndex, Length),
            {RightIndex, Array2} = find_small(Array1, RightTreeIndex, Length),
            % get left and right value
            LeftValue = array:get(LeftIndex, Array2),
            RightValue = array:get(RightIndex, Array2),
            % compare left and right value
            {SmallIndex, SmallValue} = small_one({LeftIndex, LeftValue},
                {RightIndex, RightValue}),
            {SmallPriority, _} = SmallValue,
            % compare small child and parent
            if ParentPriority > SmallPriority ->
                % excange value
                Array3 = exchange_node({ParentIndex, ParentValue}, 
                    {SmallIndex, SmallValue}, Array2);
                true -> 
                    Array3 = Array2
            end,
            {ParentIndex, Array3};
    % third: have left child
        true ->
            % not exist child 
            % get left value 
            LeftIndex =  LeftTreeIndex,
            LeftValue = array:get(LeftIndex, Array),
            % compare left and parent value
            {LeftPriority, _} = LeftValue,
            if ParentPriority > LeftPriority->
                % excange value
                Array1 = exchange_node({ParentIndex, ParentValue},
                    {LeftIndex, LeftValue}, Array);
                true -> Array1 = Array
            end,
            {ParentIndex, Array1}
    end.

% loop sort all arry
loop_find_small(Array, StartIndex, Length) when StartIndex >= Length ->
    {Array, StartIndex};
loop_find_small(Array, StartIndex, Length) ->
    {PIndex, Array1} = find_small(Array , StartIndex, Length),
    NextIndex = PIndex + 1,
    loop_find_small(Array1, NextIndex, Length).

%  test from_array
test_from_array() -> 
    List = [{100,0}, {1, 0}, {5, 0}, {44,0}, {0, 0}, {10,0}, {9,0}],
    Array = array:from_list(List),
    Array1 = from_array(Array),
    Array2 = array:to_list(Array1),
    io:format("~p ~n", [Array2]).