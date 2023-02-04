heapq
=====

An OTP library

Priority Queue

Build
-----

    $ rebar3 compile

API

    new/0 创建一个空的优先队列
    from_array/1: 从一个无序数组转化为一个优先队列
    pop/1: 弹出一个元素， 函数返回弹出元素和新队列
    push/2: 放入一个元素， 函数返回新的队列
