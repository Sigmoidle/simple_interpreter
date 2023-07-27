⍝ fibonacci sequence

number_of_terms ← 10

first_term ← 0
second_term ← 1
sequence ← first_term , second_term

:If (number_of_terms ≥ 0)
    :If (number_of_terms ≠ 1)
        count ← 2
        :While count < number_of_terms
            sequence ← sequence , (+/((((≢sequence) - 1),(≢sequence))⊇sequence))
            count ← count + 1
        :EndWhile
    :Else
        sequence ← 1
    :End
:Else :End

sequence