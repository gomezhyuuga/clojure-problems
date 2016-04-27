; Unit tests for the Relational Algebra DSL.

(use 'clojure.test)

(def s1 (relation :students1))
(def s2 (relation :students2))
(def c (relation :courses))
(def e (relation :enrollments))
(def p (relation :pizzas))

(deftest test-relation
  (is (=
        (str "+-----+------------------+-----+\n"
             "| id  | name             | age |\n"
             "+-----+------------------+-----+\n"
             "| 199 | Gwen Stacy       |  18 |\n"
             "| 286 | Natalia Romanova |  25 |\n"
             "| 417 | Tony Stark       |  45 |\n"
             "| 430 | Peggy Carter     |  91 |\n"
             "| 447 | Peter Parker     |  18 |\n"
             "| 505 | Pepper Potts     |  32 |\n"
             "+-----+------------------+-----+")
        (str s1)))
  (is (=
        (str "+-----+------------------+------+\n"
             "| id  | name             | age  |\n"
             "+-----+------------------+------+\n"
             "| 286 | Natalia Romanova |   25 |\n"
             "| 430 | Peggy Carter     |   91 |\n"
             "| 505 | Pepper Potts     |   32 |\n"
             "| 528 | Steve Rogers     |   93 |\n"
             "| 559 | Jane Foster      |   29 |\n"
             "| 598 | MaryJane Watson  |   18 |\n"
             "| 666 | Damien Thorn     |   66 |\n"
             "| 824 | Bruce Banner     |   42 |\n"
             "| 993 | Diana Prince     | 3217 |\n"
             "+-----+------------------+------+")
        (str s2)))
  (is (=
        (str "+--------+----------------------------------+--------+------+\n"
             "| code   | name                             | time   | room |\n"
             "+--------+----------------------------------+--------+------+\n"
             "| TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
             "| TC2006 | Programming Languages            | We1900 | 6307 |\n"
             "| TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
             "| TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
             "| TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
             "+--------+----------------------------------+--------+------+")
        (str c)))
  (is (=
        (str "+--------+-----+\n"
             "| code   | id  |\n"
             "+--------+-----+\n"
             "| TC2025 | 824 |\n"
             "| TC2025 | 286 |\n"
             "| TC2025 | 528 |\n"
             "| TC2006 | 528 |\n"
             "| TC2006 | 505 |\n"
             "| TC2006 | 993 |\n"
             "| TC2026 | 505 |\n"
             "| TC2026 | 666 |\n"
             "| TC2026 | 598 |\n"
             "| TC3049 | 430 |\n"
             "| TC3049 | 447 |\n"
             "| TC3049 | 666 |\n"
             "| TC3048 | 666 |\n"
             "| TC3048 | 417 |\n"
             "| TC3048 | 430 |\n"
             "+--------+-----+")
        (str e)))
  (is (= (str "+-----------+--------+\n"
              "| flavor    | size   |\n"
              "+-----------+--------+\n"
              "| Hawaiian  | Medium |\n"
              "| Pepperoni | Large  |\n"
              "| Supreme   | Small  |\n"
              "+-----------+--------+")
         (str p)))
  (is (thrown?
        IllegalArgumentException
        (relation 42))
      "Should throw exception if parameter to relation is not a keyword.")
  (is (thrown?
        java.io.FileNotFoundException
        (relation :inexistent))
      "Should throw exception if the requested file was not found."))

(deftest test-union
  (is (= (str "+-----+------------------+------+\n"
              "| id  | name             | age  |\n"
              "+-----+------------------+------+\n"
              "| 199 | Gwen Stacy       |   18 |\n"
              "| 286 | Natalia Romanova |   25 |\n"
              "| 417 | Tony Stark       |   45 |\n"
              "| 430 | Peggy Carter     |   91 |\n"
              "| 447 | Peter Parker     |   18 |\n"
              "| 505 | Pepper Potts     |   32 |\n"
              "| 528 | Steve Rogers     |   93 |\n"
              "| 559 | Jane Foster      |   29 |\n"
              "| 598 | MaryJane Watson  |   18 |\n"
              "| 666 | Damien Thorn     |   66 |\n"
              "| 824 | Bruce Banner     |   42 |\n"
              "| 993 | Diana Prince     | 3217 |\n"
              "+-----+------------------+------+")
         (str (union s1 s2))))
  (is (= (str "+-----+------------------+------+\n"
              "| id  | name             | age  |\n"
              "+-----+------------------+------+\n"
              "| 286 | Natalia Romanova |   25 |\n"
              "| 430 | Peggy Carter     |   91 |\n"
              "| 505 | Pepper Potts     |   32 |\n"
              "| 528 | Steve Rogers     |   93 |\n"
              "| 559 | Jane Foster      |   29 |\n"
              "| 598 | MaryJane Watson  |   18 |\n"
              "| 666 | Damien Thorn     |   66 |\n"
              "| 824 | Bruce Banner     |   42 |\n"
              "| 993 | Diana Prince     | 3217 |\n"
              "| 199 | Gwen Stacy       |   18 |\n"
              "| 417 | Tony Stark       |   45 |\n"
              "| 447 | Peter Parker     |   18 |\n"
              "+-----+------------------+------+")
         (str (union s2 s1))))
  (is (thrown?
        IllegalArgumentException
        (union c e))
      "Should throw exception if attributes in both relations are not union-compatible (have the same name and same order).")
  (is (thrown?
        IllegalArgumentException
        (union c 42))
      "Should throw exception if a parameter is a not a relation."))

(deftest test-difference
  (is (= (str  "+-----+--------------+-----+\n"
               "| id  | name         | age |\n"
               "+-----+--------------+-----+\n"
               "| 199 | Gwen Stacy   |  18 |\n"
               "| 417 | Tony Stark   |  45 |\n"
               "| 447 | Peter Parker |  18 |\n"
               "+-----+--------------+-----+")
         (str (difference s1 s2))))
  (is (= (str "+-----+-----------------+------+\n"
              "| id  | name            | age  |\n"
              "+-----+-----------------+------+\n"
              "| 528 | Steve Rogers    |   93 |\n"
              "| 559 | Jane Foster     |   29 |\n"
              "| 598 | MaryJane Watson |   18 |\n"
              "| 666 | Damien Thorn    |   66 |\n"
              "| 824 | Bruce Banner    |   42 |\n"
              "| 993 | Diana Prince    | 3217 |\n"
              "+-----+-----------------+------+")
         (str (difference s2 s1))))
  (is (thrown?
        IllegalArgumentException
        (difference c e))
      "Should throw exception if attributes in both relations are not union-compatible (have the same name and same order).")
  (is (thrown?
        IllegalArgumentException
        (difference c 42))
      "Should throw exception if a parameter is a not a relation."))

(deftest test-intersection
  (is (= (str "+-----+------------------+-----+\n"
              "| id  | name             | age |\n"
              "+-----+------------------+-----+\n"
              "| 286 | Natalia Romanova |  25 |\n"
              "| 430 | Peggy Carter     |  91 |\n"
              "| 505 | Pepper Potts     |  32 |\n"
              "+-----+------------------+-----+")
         (str (intersection s1 s2))))
  (is (= (str "+--------+----------------------------------+--------+------+\n"
              "| code   | name                             | time   | room |\n"
              "+--------+----------------------------------+--------+------+\n"
              "| TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "+--------+----------------------------------+--------+------+")
         (str (intersection c c))))
  (is (thrown?
        IllegalArgumentException
        (intersection c e))
      "Should throw exception if attributes in both relations are not union-compatible (have the same name and same order).")
  (is (thrown?
        IllegalArgumentException
        (intersection c 42))
      "Should throw exception if a parameter is a not a relation."))

(deftest test-product
  (is (= (str "+-----+------------------+-----+-----------+--------+\n"
              "| id  | name             | age | flavor    | size   |\n"
              "+-----+------------------+-----+-----------+--------+\n"
              "| 199 | Gwen Stacy       |  18 | Hawaiian  | Medium |\n"
              "| 199 | Gwen Stacy       |  18 | Pepperoni | Large  |\n"
              "| 199 | Gwen Stacy       |  18 | Supreme   | Small  |\n"
              "| 286 | Natalia Romanova |  25 | Hawaiian  | Medium |\n"
              "| 286 | Natalia Romanova |  25 | Pepperoni | Large  |\n"
              "| 286 | Natalia Romanova |  25 | Supreme   | Small  |\n"
              "| 417 | Tony Stark       |  45 | Hawaiian  | Medium |\n"
              "| 417 | Tony Stark       |  45 | Pepperoni | Large  |\n"
              "| 417 | Tony Stark       |  45 | Supreme   | Small  |\n"
              "| 430 | Peggy Carter     |  91 | Hawaiian  | Medium |\n"
              "| 430 | Peggy Carter     |  91 | Pepperoni | Large  |\n"
              "| 430 | Peggy Carter     |  91 | Supreme   | Small  |\n"
              "| 447 | Peter Parker     |  18 | Hawaiian  | Medium |\n"
              "| 447 | Peter Parker     |  18 | Pepperoni | Large  |\n"
              "| 447 | Peter Parker     |  18 | Supreme   | Small  |\n"
              "| 505 | Pepper Potts     |  32 | Hawaiian  | Medium |\n"
              "| 505 | Pepper Potts     |  32 | Pepperoni | Large  |\n"
              "| 505 | Pepper Potts     |  32 | Supreme   | Small  |\n"
              "+-----+------------------+-----+-----------+--------+")
         (str (product s1 p))))
  (is (= (str "+-----------+--------+--------+----------------------------------+--------+------+\n"
              "| flavor    | size   | code   | name                             | time   | room |\n"
              "+-----------+--------+--------+----------------------------------+--------+------+\n"
              "| Hawaiian  | Medium | TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| Hawaiian  | Medium | TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| Hawaiian  | Medium | TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| Hawaiian  | Medium | TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| Hawaiian  | Medium | TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "| Pepperoni | Large  | TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| Pepperoni | Large  | TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| Pepperoni | Large  | TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| Pepperoni | Large  | TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| Pepperoni | Large  | TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "| Supreme   | Small  | TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| Supreme   | Small  | TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| Supreme   | Small  | TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| Supreme   | Small  | TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| Supreme   | Small  | TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "+-----------+--------+--------+----------------------------------+--------+------+")
         (str (product p c))))
  (is (thrown?
        IllegalArgumentException
        (product c 42))
      "Should throw exception if a parameter is a not a relation.")
  (is (thrown?
        IllegalArgumentException
        (product c s1))
      "Should throw exception if attributes in both relations are not distinct."))

(deftest test-project
  (is (= (str "+-----+\n"
              "| age |\n"
              "+-----+\n"
              "|  18 |\n"
              "|  25 |\n"
              "|  45 |\n"
              "|  91 |\n"
              "|  32 |\n"
              "+-----+")
         (str (project [:age] s1))))
  (is (= (str "+------+----------------------------------+\n"
              "| room | name                             |\n"
              "+------+----------------------------------+\n"
              "| 5204 | Advanced Programming             |\n"
              "| 6307 | Programming Languages            |\n"
              "| 4207 | Web Applications Development     |\n"
              "| 4310 | Software Design and Architecture |\n"
              "| 4202 | Compiler Design                  |\n"
              "+------+----------------------------------+")
         (str (project [:room :name] c))))
  (is (= (str "+--------+\n"
              "| code   |\n"
              "+--------+\n"
              "| TC2025 |\n"
              "| TC2006 |\n"
              "| TC2026 |\n"
              "| TC3049 |\n"
              "| TC3048 |\n"
              "+--------+")
         (str (project [:code] e))))
  (is (= (str "+-----+------------------+-----+\n"
              "| id  | name             | age |\n"
              "+-----+------------------+-----+\n"
              "| 199 | Gwen Stacy       |  18 |\n"
              "| 286 | Natalia Romanova |  25 |\n"
              "| 417 | Tony Stark       |  45 |\n"
              "| 430 | Peggy Carter     |  91 |\n"
              "| 447 | Peter Parker     |  18 |\n"
              "| 505 | Pepper Potts     |  32 |\n"
              "+-----+------------------+-----+")
         (str (project [:id :name :age] s1))))
  (is (thrown?
        IllegalArgumentException
        (project '(:code) c))
      "Should throw exception if first parameter is not a vector.")
  (is (thrown?
        IllegalArgumentException
        (project [] c))
      "Should throw exception if attribute vector is empty.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :code 'time :room] c))
      "Should throw exception if in the attribute vector there's a non-keyword.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :code :name :time] c))
      "Should throw exception if the attribute vector has repeated values.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :inexistent :code] c))
      "Should throw exception if the attribute vector includes an attribute that does not exist in the relation.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :code] 42))
      "Should throw exception if the second parameter is not a relation."))

(deftest test-rename
  (is (= (str "+------------+------------------+-------------+\n"
              "| student-id | student-name     | student-age |\n"
              "+------------+------------------+-------------+\n"
              "|        199 | Gwen Stacy       |          18 |\n"
              "|        286 | Natalia Romanova |          25 |\n"
              "|        417 | Tony Stark       |          45 |\n"
              "|        430 | Peggy Carter     |          91 |\n"
              "|        447 | Peter Parker     |          18 |\n"
              "|        505 | Pepper Potts     |          32 |\n"
              "+------------+------------------+-------------+")
         (str (rename [:student-id :student-name :student-age] s1))))
  (is (= (str "+--------+----------------------------------+--------+------+\n"
              "| a      | b                                | c      | d    |\n"
              "+--------+----------------------------------+--------+------+\n"
              "| TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "+--------+----------------------------------+--------+------+")
         (str (rename [:a :b :c :d] c))))
  (is (thrown?
        IllegalArgumentException
        (rename '(:c :n :t :r) c))
      "Should throw exception if first parameter is not a vector.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n 't :r] c))
      "Should throw exception if in the attribute vector there's a non-keyword.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n :c :r] c))
      "Should throw exception if the attribute vector has repeated values.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n :r] c))
      "Should throw exception if the number of attributes in the attribute vector and in the relation are not the same.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n :t :r] 42))
      "Should throw exception if the second parameter is not a relation."))

(deftest test-select
  (is (= (str "+-----+------------+-----+\n"
              "| id  | name       | age |\n"
              "+-----+------------+-----+\n"
              "| 417 | Tony Stark |  45 |\n"
              "+-----+------------+-----+")
         (str (select (= :id 417) s1))))
  (is (= (str "+-----+--------------+-----+\n"
              "| id  | name         | age |\n"
              "+-----+--------------+-----+\n"
              "| 199 | Gwen Stacy   |  18 |\n"
              "| 447 | Peter Parker |  18 |\n"
              "| 505 | Pepper Potts |  32 |\n"
              "+-----+--------------+-----+")
         (str (select (or (< :age 20) (> :id 500)) s1))))
  (is (= (str "+--------+-----------------------+--------+------+\n"
              "| code   | name                  | time   | room |\n"
              "+--------+-----------------------+--------+------+\n"
              "| TC2025 | Advanced Programming  | Mo1600 | 5204 |\n"
              "| TC2006 | Programming Languages | We1900 | 6307 |\n"
              "+--------+-----------------------+--------+------+")
         (str (select
                (or (= :code "TC2025")
                    (and (zero? (rem :room 7))
                         (= "We" (subs :time 0 2))))
                c))))
  (is (thrown?
        IllegalArgumentException
        (select (= :inexistent 42)  s1) )
      "Should throw exception if using in select expression an attribute that does not exist in relation.")
  (is (thrown?
        IllegalArgumentException
        (select (= :age 18) 42))
      "Should throw exception if the second parameter is not a relation."))

(deftest test-composed-1
  "In what room and at what time is the 'Web Applications Development' course taught?"
  (is (= (str "+------+--------+\n"
              "| room | time   |\n"
              "+------+--------+\n"
              "| 4207 | Tu1900 |\n"
              "+------+--------+")
         (str (->>
                (select (= :name "Web Applications Development") c)
                (project [:room :time]))))))

(deftest test-composed-2
  "List the name of all the students who are between 45 and 100 years old."
  (is (= (str "+--------------+\n"
              "| name         |\n"
              "+--------------+\n"
              "| Tony Stark   |\n"
              "| Peggy Carter |\n"
              "| Steve Rogers |\n"
              "| Damien Thorn |\n"
              "+--------------+")
         (str (->>
                (union s1 s2)
                (select (<= 45 :age 100))
                (project [:name]))))))

(deftest test-composed-3
  "For two pizzas, list all the possible flavor permutations."
  (is (= (str "+-----------+-----------+\n"
              "| pizza-1   | pizza-2   |\n"
              "+-----------+-----------+\n"
              "| Hawaiian  | Hawaiian  |\n"
              "| Hawaiian  | Pepperoni |\n"
              "| Hawaiian  | Supreme   |\n"
              "| Pepperoni | Hawaiian  |\n"
              "| Pepperoni | Pepperoni |\n"
              "| Pepperoni | Supreme   |\n"
              "| Supreme   | Hawaiian  |\n"
              "| Supreme   | Pepperoni |\n"
              "| Supreme   | Supreme   |\n"
              "+-----------+-----------+")
         (str (->>
                (project [:flavor] p)
                (rename [:f])
                (product p)
                (project [:flavor :f])
                (rename [:pizza-1 :pizza-2]))))))

(deftest test-composed-4
  "List the name of all the courses in which 'Damien Thorn' is enrolled."
  (is (= (str "+----------------------------------+\n"
              "| name                             |\n"
              "+----------------------------------+\n"
              "| Web Applications Development     |\n"
              "| Software Design and Architecture |\n"
              "| Compiler Design                  |\n"
              "+----------------------------------+")
         (str (->>
                (union s1 s2)
                (select (= :name "Damien Thorn"))
                (project [:id])
                (rename [:s-id])
                (product e)
                (select (= :id :s-id))
                (project [:code])
                (rename [:e-code])
                (product c)
                (select (= :code :e-code))
                (project [:name]))))))

(deftest test-composed-5
  "List the name of all the students who are not enrolled in the 'Programming Languages' course."
  (is (= (str "+------------------+\n"
              "| name             |\n"
              "+------------------+\n"
              "| Gwen Stacy       |\n"
              "| Natalia Romanova |\n"
              "| Tony Stark       |\n"
              "| Peggy Carter     |\n"
              "| Peter Parker     |\n"
              "| Jane Foster      |\n"
              "| MaryJane Watson  |\n"
              "| Damien Thorn     |\n"
              "| Bruce Banner     |\n"
              "+------------------+")
         (str (let [s (union s1 s2)]
                (->>
                  (select (= :name "Programming Languages") c)
                  (project [:code])
                  (rename [:c-code])
                  (product e)
                  (select (= :code :c-code))
                  (project [:id])
                  (rename [:e-id])
                  (product s)
                  (select (= :id :e-id))
                  (project [:id :name :age])
                  (difference s)
                  (project [:name])))))))

(deftest test-composed-6
  "List the id and name of all the students that are enrolled in both the 'Compiler Design' and 'Software Design and Architecture' courses."
  (is (= (str "+-----+--------------+\n"
              "| id  | name         |\n"
              "+-----+--------------+\n"
              "| 430 | Peggy Carter |\n"
              "| 666 | Damien Thorn |\n"
              "+-----+--------------+")
         (str (let [s (union s1 s2)
                    students-by-course
                     (fn [course-name]
                       (->>
                         (select (= :name course-name) c)
                         (project [:code])
                         (rename [:c-code])
                         (product e)
                         (select (= :code :c-code))
                         (project [:id])
                         (rename [:e-id])
                         (product s)
                         (select (= :id :e-id))
                         (project [:id :name])))]
                (->>
                  (intersection
                    (students-by-course "Compiler Design")
                    (students-by-course "Software Design and Architecture"))))))))

(run-tests)
