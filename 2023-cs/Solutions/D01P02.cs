using System.Data;

namespace AoC_2023.Solutions;


// --- Day 1: Trebuchet?! - Part 2 --- //


public class D01P02 : Solution
{
    public override string ExampleAnswer => "281";
    public override string Answer => "54504";

    private readonly List<State> states =
        new[] { "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" }
            .Select((n, i) => new State(n, i + 1))
            .ToList();
    
    public override string Solve(IEnumerable<string> lines)
    {
        return lines
            .ToList()
            .Select(FindDigits)
            .Sum()
            .ToString();
    }
    
    private int FindDigits(string l)
    {
        var firstDigit = 0;
        var lastDigit = 0;
        
        foreach (var c in l)
        {
            if (IsDigit(c))
            {
                if (firstDigit == 0) firstDigit = c - '0';
                lastDigit = c - '0';
                
                ResetAllStates();
            }
            else
            {
                foreach (var digit in states.Select(s => s.Check(c)))
                {
                    if (digit is null) continue;
                    
                    if (firstDigit == 0) firstDigit = digit.Value;
                    lastDigit = digit.Value;
                }
            }
        }
        
        var result = firstDigit * 10 + lastDigit;
        
        ResetAllStates();
        
        return result;
    }

    private void ResetAllStates() => states.ForEach(s => s.Reset());
    
    private static bool IsDigit(char c) => c is >= '0' and <= '9';

    private class State
    {
        private readonly string numberName;
        private readonly int number;

        private int position;

        public State(string numberName, int number)
        {
            this.numberName = numberName;
            this.number = number;
        }

        public int? Check(char c)
        {
            if (c != numberName[position])
            {
                if (c == numberName[0]) position = 1;
                else Reset();
                
                return null;
            }
            
            position++;
            
            if (position == numberName.Length)
            {
                Reset();
                return number;
            }

            return null;
        }

        public void Reset() => position = 0;
    }
}
