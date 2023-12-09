# Day 6

This question is just a math problem.

Let \\(h\\) be how much time you hold the button and \\(T\\) be the total time you
have. Then, since you leave at \\(h\\) speed, you walk for \\(h\\cdot (T-h)\\) units
of distance.

Now, for each pair \\(\(T, d\)\\), you would need to find all values of \\(t\\) such
that \\(t \\cdot (T-t) > d\\).

If we want to just count (which is more than enough for the problem), we just need
to filter in the range of \\(0..T\\) for those values and then count them.

However, the result can also be calculated explicitly. For that, we just need to find
the values of \\(t_1, t_2\\) for which \\(t \\cdot (T-t) = d\\), i.e.
\\(t \\cdot (T-t) - d = 0\\) and then the range will be the integral values that
are between \\(t_1\\) and \\(t_2\\).
