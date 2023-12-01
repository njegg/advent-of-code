namespace AoC_2023.Solutions;

public class SolutionBase
{
    public virtual string Solve(string inputPath) => "-";
    protected virtual string ExampleSolution => "-";

    public bool Test(string result) => result == ExampleSolution;
}