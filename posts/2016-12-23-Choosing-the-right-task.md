---
title: Which task should I choose?
date: 2016-12-23 20:59:48
tags: en, organization
description: Choosing the correct task is completely important for planning your day.
---

# Introduction
When I defined the tasks that would be necessary to accomplish my goals, I didn't
knew which tasks should be done first. After reading a few blog posts I defined
the way to do it. If you want to know how, keep reading.

# Choose the right parameters
There are three parameters I chose: **importance**, **urgency**, and **time
required**. All these parameters give me a value that is used to calculate the
total weight of the task.

## Importance
I defined a number line for this parameter. If the task is not very important,
for example something related to a personal project, chores, etc. It should be
located between 1 and 5. Otherwise, if it is really important, like: homework,
an appointment, etc. Then you'll have to assign a number between 6 and 10.
This is the way I do it. However, it depends of you which task has more importance.

![**Imagen 1. Importance**][1]

## Urgency
For this parameter, I defined the urgency according to the left time I have to
complete it. And I used the fibbonaci series because is going to affect more
the total weight of the task.

|Urgency Level|	When do I have to complete it?|
|--|---|
|1|More than 1 week|
|2|One week|
|3|Five or six days|
|5|Three or four days|
|8|One or two days|
|13|Right Now!|

## Time required
I also consider this parameter an influence for the total weight.

|Time required level| How much time do I need to complete it?|
|--|---|
|1|Less than 1 hour|
|2|Between 2 and 1 hour|
|3|Between 3 and 7 hour|
|5|Between 8 and 11 hour|
|8|Between 12 and 24 hour|
|13|More than 24 hours!|

# How important is every one of them
Now, you should define a percentage of weight to every parameter:

- Urgency: 50%
- Importane: 30%
- Time Required: 20%

The percentage above is for my decision. You can define your own.

# Get the total
Now, simple calculate the following formula:

```
Weight = Urgency * 50% + Importance * 30% + Time Required * 20%
```

This value will help you to choose the next tasks. For instance, you have two tasks:

```
Task #1:
U = 6
TR = 4
I = 5
W = U*50% + TR*30% + I*20% = 4
```

```
Task #2:
U = 8
TR = 1
I = 6
W = U*50% + TR*30% + I*20% = 5
```

According to this result the task #2 should be realized before task #1. However,
what would happen if two tasks have the same weight? The you can trust in the
priority of the parameters. For me, urgency and time required are more important.
For instance, between the following two tasks:

```
Task #1:
U = 6
TR = 6
I = 5
W = U*50% + TR*30% + I*20% = 5
```

```
Task #2:
U = 8
TR = 1
I = 6
W = U*50% + TR*30% + I*20% = 5
```

I'll choose the task #2, because it has a bigger urgency. If both tasks had the
same urgency, then I'll look at the Timre Required, and so on. If all the parameter
have the same value, then you can choose any task.

# Conclusion
This is a quantitative way to evalute how the priority of a task. I trust a lot
in numbers and this method has been really good for me. I hope it works for you too.
If you have any comment, suggestion, and so on and so forth. Feel free to do it! :D
ByE!

[1]: /images/importance.png {width=600px class=img-blog}

