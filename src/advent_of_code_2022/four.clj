(ns advent-of-code-2022.four
  (:require [clojure.string :as string]))

;; --- Day 4: Camp Cleanup ---

;; Space needs to be cleared before the last supplies can be unloaded from the ships, and so several Elves have been assigned the job of cleaning up sections of the camp. Every section has a unique ID number, and each Elf is assigned a range of section IDs.

;; However, as some of the Elves compare their section assignments with each other, they've noticed that many of the assignments overlap. To try to quickly find overlaps and reduce duplicated effort, the Elves pair up and make a big list of the section assignments for each pair (your puzzle input).

;; For example, consider the following list of section assignment pairs:

;; 2-4,6-8
;; 2-3,4-5
;; 5-7,7-9
;; 2-8,3-7
;; 6-6,4-6
;; 2-6,4-8
;; For the first few pairs, this list means:

;; Within the first pair of Elves, the first Elf was assigned sections 2-4 (sections 2, 3, and 4), while the second Elf was assigned sections 6-8 (sections 6, 7, 8).
;; The Elves in the second pair were each assigned two sections.
;; The Elves in the third pair were each assigned three sections: one got sections 5, 6, and 7, while the other also got 7, plus 8 and 9.
;; This example list uses single-digit section IDs to make it easier to draw; your actual list might contain larger numbers. Visually, these pairs of section assignments look like this:

;; .234.....  2-4
;; .....678.  6-8

;; .23......  2-3
;; ...45....  4-5

;; ....567..  5-7
;; ......789  7-9

;; .2345678.  2-8
;; ..34567..  3-7

;; .....6...  6-6
;; ...456...  4-6

;; .23456...  2-6
;; ...45678.  4-8
;; Some of the pairs have noticed that one of their assignments fully contains the other. For example, 2-8 fully contains 3-7, and 6-6 is fully contained by 4-6. In pairs where one assignment fully contains the other, one Elf in the pair would be exclusively cleaning sections their partner will already be cleaning, so these seem like the most in need of reconsideration. In this example, there are 2 such pairs.

;; In how many assignment pairs does one range fully contain the other?

(def test-input "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn input->range-pairs [input]
  (string/split-lines input))

(defn range-pair->ranges [range-pair]
  (string/split range-pair #","))

(defn parse-range [range-str]
  (let [[start end] (string/split range-str #"-")
        r (range (Integer/parseInt start) (inc (Integer/parseInt end)))]
    {:min (first r)
     :max (last r)}))

(defn parse-ranges [ranges]
  (map parse-range ranges))

(defn range-contains? [range other-range]
  (and (<= (:min range) (:min other-range))
       (>= (:max range) (:max other-range))))

(defn ranges-contains? [ranges]
  (map #(or (range-contains? (first %) (second %)) (range-contains? (second %) (first %))) ranges))

(defn part-one [input]
  (as-> input $
    (input->range-pairs $)
    (map range-pair->ranges $)
    (map parse-ranges $)
    (ranges-contains? $)
    (filter true? $)
    (count $)))

(comment

  (parse-range "2-4")

  (part-one test-input))

;; part one

(def puzzle-input "34-82,33-81
59-59,69-73
6-96,98-99
1-94,3-96
13-92,20-64
37-75,76-93
5-98,6-6
40-65,40-64
13-63,84-91
31-75,31-35
83-96,86-96
65-85,64-85
85-85,36-86
71-73,72-81
3-91,91-92
45-72,45-73
42-70,42-58
45-45,16-45
2-59,49-58
46-59,60-60
21-21,21-77
65-72,69-72
3-77,3-76
2-91,23-89
86-92,7-86
25-86,24-86
11-62,1-62
23-30,23-23
31-94,27-29
58-90,59-89
45-70,19-46
56-56,57-97
65-84,67-83
47-77,44-48
1-68,29-69
18-18,19-68
99-99,72-93
3-60,1-60
31-31,32-74
2-2,7-62
25-83,10-24
7-96,16-47
12-71,13-70
56-57,20-56
18-24,25-25
58-89,58-90
63-68,18-63
26-70,69-71
95-98,8-96
21-94,22-94
22-91,21-91
16-22,20-23
94-95,3-94
2-20,19-80
26-94,6-17
30-82,29-82
22-73,22-71
51-62,50-62
10-65,9-64
35-36,1-35
28-90,39-90
21-22,20-21
2-35,3-34
10-79,79-79
37-80,80-81
7-93,99-99
4-7,32-61
2-37,1-38
4-91,3-87
32-92,31-91
64-64,64-83
7-87,3-8
60-91,55-59
35-41,35-46
46-87,70-87
3-14,4-88
44-60,19-93
10-31,10-74
25-87,24-26
80-97,90-91
3-15,6-23
18-83,49-77
7-45,46-90
73-91,11-79
2-4,3-99
66-98,7-96
62-63,61-62
30-97,97-98
3-79,78-80
15-32,30-72
66-85,84-85
35-76,45-76
14-65,14-65
81-82,42-81
3-90,4-90
3-4,5-99
63-99,1-99
65-66,9-65
60-84,85-85
17-17,6-18
73-75,7-74
57-98,56-99
59-59,10-58
10-18,20-52
36-78,35-78
1-99,99-99
21-88,51-55
42-97,42-96
7-79,8-78
32-71,72-96
19-35,43-73
2-12,4-11
91-91,39-90
49-86,50-89
13-19,21-84
60-81,67-81
17-26,26-75
59-95,60-95
30-70,21-70
7-60,8-61
10-95,95-97
20-22,21-84
11-98,6-9
2-24,3-26
41-50,62-99
13-25,26-99
35-43,27-36
99-99,42-81
86-86,8-85
13-46,13-84
76-88,77-87
19-77,4-77
20-54,21-44
19-91,90-91
8-67,7-67
8-74,29-73
14-16,15-25
64-70,70-71
16-99,16-16
6-94,5-95
87-98,71-98
1-60,7-60
1-76,1-96
31-92,24-30
12-81,9-11
4-85,2-78
11-84,8-9
4-4,8-90
6-11,12-82
2-96,2-70
12-91,12-97
79-89,2-89
75-75,67-74
5-81,5-5
68-74,74-84
81-82,82-95
5-78,6-78
53-87,38-88
83-94,75-88
53-79,31-48
55-57,56-88
3-69,69-70
5-54,2-4
48-85,28-47
23-78,22-63
13-20,29-96
24-45,86-88
43-64,44-71
7-84,49-84
16-89,15-88
6-73,13-73
71-71,53-70
4-12,11-41
36-53,37-52
46-91,45-91
5-58,4-6
94-94,95-97
9-55,55-94
6-49,5-7
4-99,3-5
92-93,91-93
61-83,62-84
22-82,23-81
8-94,7-94
7-9,12-63
1-51,73-73
7-66,2-6
44-45,7-44
8-94,8-99
90-97,96-97
6-72,72-91
55-60,59-62
2-40,1-3
51-75,36-75
6-8,7-86
1-1,2-97
25-88,89-89
19-94,18-20
3-3,4-98
32-68,52-67
10-54,7-54
17-69,18-70
17-75,8-54
53-65,16-52
50-88,49-49
8-63,3-7
35-38,36-84
50-54,14-54
17-17,18-62
20-26,27-83
3-3,4-84
9-25,41-56
16-72,17-71
22-89,5-89
12-30,12-30
78-94,55-78
40-83,85-88
49-84,48-50
96-98,1-96
78-78,76-77
81-82,22-34
93-94,5-92
39-70,38-40
80-84,85-85
3-73,4-91
13-49,13-89
2-94,2-25
18-72,71-75
27-69,25-94
20-78,16-79
25-29,25-50
75-95,93-95
35-65,34-65
58-91,57-90
95-95,7-95
31-31,31-81
8-88,8-89
90-90,4-90
13-66,37-66
17-17,18-19
74-74,39-73
28-30,29-31
22-95,21-94
12-16,11-15
76-89,74-74
28-91,29-92
23-23,23-96
2-99,98-99
84-90,7-83
17-19,20-20
14-99,13-93
39-81,40-81
6-55,5-7
63-63,11-62
93-93,3-92
69-79,4-69
6-99,4-7
65-87,60-88
14-98,13-99
10-58,11-64
37-95,37-91
24-95,99-99
51-94,8-94
3-61,1-1
2-94,23-88
18-86,17-86
47-96,29-48
11-79,24-79
26-87,25-25
10-70,9-70
21-72,71-85
29-69,69-90
17-64,17-63
34-83,8-33
4-52,53-75
89-99,2-90
2-89,2-88
11-34,9-87
19-95,20-95
32-67,32-67
40-99,96-99
29-87,87-95
11-61,5-60
3-96,1-80
12-87,57-88
34-93,93-98
36-76,35-37
2-70,2-71
1-5,8-58
27-91,27-27
1-60,1-61
26-80,27-81
5-91,92-94
82-82,63-81
35-99,36-99
8-8,8-52
93-96,5-94
11-18,17-94
17-81,17-85
42-66,43-66
23-83,22-24
47-60,46-48
81-82,5-81
58-89,58-90
10-48,11-83
12-93,11-93
98-99,27-94
21-61,18-19
95-95,52-95
18-99,17-95
19-75,15-75
43-44,6-44
63-86,62-64
85-90,30-62
2-78,77-78
13-72,87-96
62-86,49-61
5-33,15-34
26-71,59-83
2-85,76-87
8-14,15-72
19-77,17-76
4-68,2-2
23-92,22-91
15-72,4-72
8-70,8-46
8-74,8-83
9-75,29-58
14-97,13-96
9-61,9-61
9-10,13-44
16-48,40-49
73-78,79-79
32-54,54-54
13-99,13-98
9-91,8-91
8-42,12-42
10-87,9-86
5-5,6-93
8-75,9-76
20-94,17-99
16-17,17-58
70-88,89-89
68-68,64-71
23-94,74-94
14-58,14-57
69-70,3-70
24-82,22-82
19-97,12-18
4-5,7-72
23-79,79-80
61-83,61-88
37-49,34-36
86-98,86-99
43-84,44-85
96-96,2-89
85-86,4-85
41-43,24-43
18-72,18-65
7-7,6-93
20-94,93-99
24-92,23-93
58-82,83-83
65-71,69-71
63-95,96-96
42-51,42-52
22-81,80-90
2-96,1-96
43-65,44-66
77-77,50-76
16-91,15-92
41-83,82-95
37-94,21-34
37-98,97-98
1-1,1-74
17-22,18-23
24-56,57-57
7-15,1-6
42-57,37-57
8-96,8-96
33-97,63-88
6-7,5-84
36-97,83-96
35-37,36-79
4-98,4-99
77-77,34-76
30-54,31-62
43-64,34-64
51-90,53-97
58-80,38-64
71-86,72-86
18-88,28-87
3-80,2-79
23-66,32-66
64-81,62-64
3-89,2-4
94-96,11-81
15-91,15-92
15-78,16-77
1-99,2-98
4-98,1-5
31-59,11-30
21-59,20-58
32-71,84-88
96-96,7-83
6-66,67-67
20-43,21-43
16-95,17-96
34-41,35-41
25-84,84-95
29-31,30-91
96-97,14-96
82-86,55-83
55-55,29-54
34-90,13-35
58-81,57-81
38-90,19-38
26-67,67-99
26-86,25-31
16-99,10-15
2-47,1-46
6-74,73-81
26-93,3-93
20-44,21-45
2-69,2-70
5-95,5-95
75-86,11-74
33-83,84-84
62-69,63-70
41-57,58-58
9-14,10-14
77-77,1-77
46-65,66-89
69-75,69-69
49-92,49-98
27-50,22-22
15-78,16-95
12-83,12-92
4-38,2-38
26-87,25-86
86-86,20-86
97-98,3-97
16-16,16-84
14-21,28-79
57-92,91-92
3-5,4-79
80-84,42-94
45-96,95-96
28-28,28-80
58-76,74-84
29-69,69-70
1-67,66-67
34-66,34-67
99-99,3-86
50-79,26-49
92-97,91-91
74-75,61-74
2-50,1-76
55-80,43-53
8-89,1-1
18-57,40-57
72-94,93-97
21-56,22-81
22-24,32-83
6-77,7-76
6-60,7-60
94-95,37-94
11-90,6-90
5-81,5-80
1-4,5-78
2-51,52-52
13-96,13-96
9-26,28-91
2-90,63-89
71-97,6-96
50-87,51-88
82-93,82-92
11-22,2-32
36-88,99-99
25-99,1-94
8-98,99-99
11-42,10-12
19-90,91-91
51-63,52-85
80-90,89-90
30-55,31-55
14-85,15-85
31-92,30-30
2-96,1-97
71-85,27-55
65-67,66-67
32-96,31-95
24-24,25-88
7-84,6-6
56-74,55-57
76-80,74-75
17-17,17-92
36-46,45-47
60-60,2-59
17-53,53-87
30-96,30-92
10-46,5-7
5-99,2-3
1-49,1-48
85-88,10-88
33-94,94-97
94-95,94-95
19-52,36-51
41-96,40-97
2-78,77-78
80-80,21-80
7-17,17-17
4-47,30-47
16-16,8-15
33-95,33-94
39-89,90-96
61-83,30-83
25-76,68-75
8-50,8-50
17-86,12-18
9-48,48-56
28-99,28-99
44-96,97-97
4-74,4-73
48-90,47-89
35-46,36-47
8-57,9-57
45-70,98-99
58-66,42-57
8-18,3-3
10-97,9-98
3-93,2-89
14-19,21-90
85-85,27-86
27-96,7-26
53-53,54-96
30-91,29-92
91-91,98-98
96-96,29-95
3-82,1-2
31-32,1-32
74-95,24-95
9-10,9-71
21-23,22-99
98-99,1-99
50-92,39-50
19-89,19-90
84-88,26-88
83-84,5-84
13-30,30-31
60-75,61-77
23-60,61-93
12-85,4-7
9-95,10-95
39-71,39-71
29-55,28-54
3-92,88-92
17-80,10-73
2-60,3-61
39-88,78-88
1-39,6-44
5-26,6-25
88-96,8-96
1-96,1-1
14-60,60-61
7-76,2-5
44-94,44-94
75-75,75-96
5-67,67-68
19-35,6-34
3-95,3-3
27-62,27-62
34-99,3-99
46-89,47-88
46-71,28-36
15-56,14-96
4-92,3-5
67-68,25-68
6-86,3-4
17-81,18-85
6-6,6-53
2-2,1-89
51-52,44-51
59-59,28-58
41-53,52-81
27-91,27-27
70-78,71-78
16-94,17-23
6-51,5-51
55-68,55-69
97-99,7-94
5-90,89-90
16-46,15-46
12-99,9-98
17-94,11-67
3-98,99-99
80-88,36-80
9-87,8-86
51-62,50-63
82-82,82-84
4-97,96-96
46-94,99-99
51-68,51-67
59-80,53-80
40-57,39-56
22-84,27-85
84-84,27-85
46-62,45-61
16-58,16-58
54-61,47-98
61-65,63-63
26-38,27-39
8-85,9-86
3-91,2-91
8-70,8-8
73-85,86-86
15-47,14-46
1-89,67-92
8-78,53-79
15-87,15-86
60-70,71-99
66-71,20-75
6-93,5-74
26-81,26-82
27-64,27-63
9-42,10-25
11-91,91-91
39-78,38-78
25-95,24-96
4-56,8-56
41-47,41-66
79-81,25-79
5-64,30-34
20-98,24-66
36-67,66-94
31-40,11-95
22-88,88-89
2-78,79-79
77-78,45-78
5-81,5-80
7-72,8-71
41-41,41-53
14-40,15-40
4-61,6-61
25-51,24-50
10-87,11-92
63-63,63-64
19-21,12-22
28-96,95-99
91-92,10-92
4-70,3-40
60-82,59-81
18-35,18-36
41-76,77-77
52-95,51-95
34-44,30-44
73-82,80-82
25-81,3-26
61-68,62-68
50-98,49-98
12-47,25-98
29-53,29-88
32-43,44-68
10-90,9-90
58-78,59-78
17-19,18-62
59-89,66-90
94-95,23-94
94-94,31-93
7-89,7-76
62-74,43-75
16-95,18-89
43-71,32-37
31-57,19-57
12-25,21-33
71-72,72-94
67-67,22-66
16-86,21-86
13-69,13-69
4-96,97-97
84-84,60-84
18-93,17-94
5-46,5-45
51-65,52-64
11-89,12-88
3-23,1-23
36-88,35-88
13-36,37-83
15-16,15-39
8-86,7-79
24-99,38-99
46-51,47-72
23-40,40-43
86-86,45-85
21-60,16-60
4-55,3-5
79-96,68-96
2-3,5-95
17-77,17-78
36-88,36-88
43-88,40-54
74-90,73-89
6-96,6-95
3-19,64-70
17-72,2-6
43-82,30-83
23-72,22-72
25-91,26-90
13-95,55-95
20-58,22-58
21-57,47-71
25-86,24-87
8-99,9-99
2-3,3-96
48-48,22-48
5-83,1-4
27-61,84-91
3-70,52-90
37-82,37-85
24-54,55-65
81-82,3-82
17-80,79-93
59-59,60-95
30-31,32-32
10-99,5-9
23-53,24-53
13-85,1-12
10-99,2-2
49-78,78-87
97-98,5-97
45-74,46-74
16-92,16-82
13-86,13-51
27-29,28-36
28-90,28-89
2-99,1-1
15-73,14-73
56-96,20-55
12-84,85-85
85-86,27-85
10-92,11-77
19-33,34-98
15-26,6-14
8-78,5-85
6-95,7-59
1-3,2-65
82-99,33-82
48-77,21-27
8-57,15-27
1-97,2-99
12-63,1-90
25-75,58-75
92-98,24-43
8-10,9-98
13-25,44-87
18-58,18-78
11-13,12-61
39-55,11-56
70-94,31-70
6-20,7-19
9-63,63-66
1-5,13-90
40-70,40-71
31-75,54-94
5-95,5-94
17-37,18-21
3-57,29-57
99-99,81-97
11-92,11-91
29-79,49-78
44-85,43-84
98-98,63-98
19-82,13-18
23-93,23-93
5-90,4-6
5-96,5-97
16-21,15-15
16-69,19-24
23-59,60-80
35-77,77-88
72-95,6-73
7-88,7-88
15-87,14-87
4-54,4-44
67-73,34-44
55-55,55-77
16-82,81-82
34-93,7-33
20-82,21-81
60-73,73-74
37-63,66-96
24-26,25-51
42-43,42-84
45-52,57-93
4-96,25-97
34-74,33-74
6-37,5-5
9-63,8-10
31-85,11-16
79-81,67-80
23-23,25-83
2-32,5-96
84-85,38-84
46-47,9-46
87-88,23-87
78-84,7-92
36-39,34-39
31-64,31-65
39-76,25-62
23-99,20-22
99-99,38-86
35-78,34-78
30-95,30-94
3-69,1-70
27-82,26-52
7-57,8-56
78-83,60-69
18-66,17-67
1-94,3-94
8-16,15-16
29-30,13-29
30-83,17-98
28-87,27-87
77-82,10-81
30-76,2-29
4-4,5-73
62-66,62-67
1-81,44-72
16-44,43-44
98-99,3-99
98-99,9-78
29-70,30-62
79-93,80-94
84-95,84-95
27-36,26-88
3-89,3-88
16-44,15-45
21-65,18-53
8-63,7-7
19-76,18-74
35-44,19-39
16-48,3-15
7-7,7-33
96-97,78-97
20-83,16-83
55-85,28-52
18-82,18-81
4-10,9-92
32-66,7-66
49-79,18-49
53-93,52-93
53-97,85-99
24-98,24-24
28-97,27-27
27-92,76-92
8-9,16-20
54-54,54-87
7-89,89-98
57-76,56-77
9-87,8-87
48-81,48-82
3-33,3-48
8-8,9-76
24-79,10-25
37-96,37-97
72-77,74-77
39-69,40-70
33-97,4-60
53-57,57-58
91-91,49-90
1-1,1-95
1-91,92-92
48-90,89-90
86-98,87-96
67-81,68-80
66-66,66-87
94-95,19-95
1-6,7-14
28-30,28-29
6-60,30-84
25-46,6-8
8-69,8-68
7-94,6-94
9-80,10-81
89-98,56-88
61-91,62-92
39-87,40-91
29-57,18-57
89-90,58-89
95-96,72-95
42-59,31-47
70-80,39-86
24-35,1-36
25-39,25-39
1-1,1-96
44-81,45-90
30-88,89-89
69-77,46-72
31-72,32-72
60-61,59-61
17-26,1-16
10-91,91-92
81-95,34-95
80-81,18-73
13-58,13-57
7-57,6-56
53-68,66-85
64-93,57-63
77-78,11-77
19-23,24-88
30-58,31-58
16-27,15-28
12-94,12-93
4-29,27-29
57-60,56-68
15-96,15-97
5-51,9-94
25-61,24-61
40-86,41-86
4-84,5-84
6-98,5-97
53-53,15-53
51-63,51-62
30-38,38-44
58-83,5-74
13-94,12-92
19-20,20-99
35-46,36-45
4-98,1-3
66-68,67-94
4-80,9-99
81-99,3-98
9-80,9-65
10-60,11-59
1-22,22-22
2-90,89-90
45-83,45-84
30-59,22-22
97-97,2-96
20-99,1-99
10-15,16-79
12-85,85-85
13-50,3-12
8-95,96-96
73-73,1-72
4-62,5-48
27-47,27-48
11-81,11-93
65-83,59-72
27-67,35-67
44-96,16-44
95-98,44-81
32-96,96-97
73-74,4-73
43-56,33-34
19-96,95-98
32-96,33-95
28-95,95-96
61-81,8-61
97-97,1-97
37-83,36-38
25-80,2-80
58-67,57-67
43-94,75-92
32-42,41-98
19-94,19-19
15-84,15-99
41-48,49-49
15-96,15-15
82-87,32-83
3-35,2-4
33-59,34-58
19-99,5-7
80-98,17-96
4-75,74-75
2-96,3-97
24-24,24-76
24-96,4-97
45-84,46-84
62-90,63-91
39-89,38-90
32-33,9-33
4-23,47-90
73-74,55-73
54-99,29-54
16-66,15-66
61-91,77-91
29-88,28-89
45-77,44-78
13-76,52-75
33-92,32-58
38-85,37-81
54-92,38-93
10-91,21-94
3-98,14-98
7-58,6-58
23-25,24-83
19-86,13-18
61-63,62-96
24-63,57-80")

(comment
  (part-one puzzle-input))

;; --- Part Two ---

;; It seems like there is still quite a bit of duplicate work planned. Instead, the Elves would like to know the number of pairs that overlap at all.

;; In the above example, the first two pairs (2-4,6-8 and 2-3,4-5) don't overlap, while the remaining four pairs (5-7,7-9, 2-8,3-7, 6-6,4-6, and 2-6,4-8) do overlap:

;; 5-7,7-9 overlaps in a single section, 7.
;; 2-8,3-7 overlaps all of the sections 3 through 7.
;; 6-6,4-6 overlaps in a single section, 6.
;; 2-6,4-8 overlaps in sections 4, 5, and 6.
;; So, in this example, the number of overlapping assignment pairs is 4.

;; In how many assignment pairs do the ranges overlap?

(defn range-overlaps? [{min :min max :max} {other-min :min other-max :max}]
  (or (and (<= min other-min) (<= other-min max))
      (and (<= min other-max) (<= other-max max))
      (and (<= other-min min) (<= min other-max))
      (and (<= other-min max) (<= max other-max))))

(defn part-two [input]
  (as-> input $
    (input->range-pairs $)
    (map range-pair->ranges $)
    (map parse-ranges $)
    (map #(range-overlaps? (first %) (second %)) $)
    (filter true? $)
    (count $)))

(comment
  (range-overlaps? {:min 0 :max 1} {:min 2 :max 3}) 
  (range-overlaps? {:min 0 :max 1} {:min 1 :max 3}) 
  (range-overlaps? {:min 0 :max 2} {:min 1 :max 3}) 
  (range-overlaps? {:min 0 :max 2} {:min 0 :max 3}) 
  (range-overlaps? {:min 0 :max 2} {:min 3 :max 3}) 
  (range-overlaps? {:min 0 :max 3} {:min 3 :max 3}) 
  (range-overlaps? {:min 3 :max 3} {:min 3 :max 3})
  (range-overlaps? {:min 3 :max 3} {:min 1 :max 3})
  
  (defn is-overlap? [text]
    (as-> text $
      (range-pair->ranges $)
      (parse-ranges $)
      (range-overlaps? (first $) (second $))))
  
  (is-overlap? "8-63,7-7")
  (is-overlap? "44-60,19-93")

  (as-> test-input $
    (input->range-pairs $)
    (map range-pair->ranges $)
    (map parse-ranges $)
    (map #(range-overlaps? (first %) (second %)) $)
    (filter true? $)
    (count $)) 
  
  (as-> puzzle-input $
    (input->range-pairs $)
    (map range-pair->ranges $)
    (map parse-ranges $)
    (map (fn [r] [r (range-overlaps? (first r) (second r))]) $)
    (filter #(false? (last %)) $)) 

  (part-two test-input)
  (part-two puzzle-input))