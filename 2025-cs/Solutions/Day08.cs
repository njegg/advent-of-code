using Xunit;

namespace _2025_cs.Solutions;


// --- Day 8: Playground ---  //


public record Day08() : Solver(AnswerOne: null, AnswerTwo: null)
{

    public record Point(float X, float Y, float Z)
    {
        public static float DistanceSqr(Point p, Point q) => 
            (q.X - p.X) * (q.X - p.X) + 
            (q.Y - p.Y) * (q.Y - p.Y) + 
            (q.Z - p.Z) * (q.Z - p.Z);

        public static Point Max => new(float.MaxValue, float.MaxValue, float.MaxValue);
    };

    public override Solution PartOne(IEnumerable<string> input)
    {
        var points = input.Select(p => {
            var coords = p.Split(",").Select(float.Parse).ToArray();
            return new Point(coords[0], coords[1], coords[2]);
        });

        var pointsSortedByX = points.OrderBy(p => p.X).ToArray();

        var minPair = MinDistHalfs(pointsSortedByX);
        
        
        var minDist = Point.DistanceSqr(minPair.Item1, minPair.Item2);
        
        Console.WriteLine($"{minPair}, Dist: {Math.Sqrt(minDist)}");
        Console.WriteLine($"Actual min = {MinDistBruteForce(pointsSortedByX)}");
        
        return 0;
    }

    public static (Point, Point) MinDist(Point[] points)
    {
        var minDistInHalfs = MinDistHalfs(points);

        var mid = (int)Math.Floor(points.Length / 2f);
        var midPoint = points[mid];
        

        var d = Math.Sqrt(Point.DistanceSqr(minDistInHalfs.Item1, minDistInHalfs.Item2));
        
        var candidates = points.Where(p => p.X > midPoint.X)

    }

    public static (Point, Point) MinDistHalfs(Span<Point> points)
    {
        switch (points.Length)
        {
            case 2: return (points[0], points[1]);
            case 3:
            {
                var distAB = Point.DistanceSqr(points[0], points[1]);
                var distAC = Point.DistanceSqr(points[0], points[2]);
                var distBC = Point.DistanceSqr(points[1], points[2]);

                return distAB < distAC && distAB < distBC
                    ? (points[0], points[1])
                    : distBC < distAC
                        ? (points[1], points[2])
                        : (points[0], points[2]);
            }
            case 1: throw new Exception("Cannot calculate distance from a single point");
        }

        var midIndex = (int)Math.Floor(points.Length / 2f);

        var leftMinPair = MinDistHalfs(points[..midIndex]);
        var rightMinPair = MinDistHalfs(points[midIndex..]);

        var leftMinDist = Point.DistanceSqr(leftMinPair.Item1, leftMinPair.Item2);
        var rightMinDist = Point.DistanceSqr(rightMinPair.Item1, rightMinPair.Item2);

        return leftMinDist < rightMinDist
            ? leftMinPair
            : rightMinPair;
    }

    public static (Point, Point) MinDistBruteForce(Span<Point> points)
    {
        var minDist = float.MaxValue;
        var minPair = (Point.Max, Point.Max);
    
        for (int i = 0; i < points.Length - 1; i++)
        {
            for (int j = i + 1; j < points.Length; j++)
            {
                var dist = Point.DistanceSqr(points[i], points[j]);
                if (dist < minDist)
                {
                    minDist = dist;
                    minPair = (points[i], points[j]);
                }
            }
        }

        return minPair;
    }
    
    [Fact]
    public void MinDistTest()
    {
        var points = new Day08().PartOneExamples[0].Input.Split("\n").Select(p =>
        {
            var coords = p.Split(",").Select(float.Parse).ToArray();
            return new Point(coords[0], coords[1], coords[2]);
        }).ToArray();

        var expected = MinDistHalfs(points);
        var actual = MinDistBruteForce(points);
        
        Assert.Equal(expected, actual);
    }
    
    public override Solution PartTwo(IEnumerable<string> input)
    {
        return "";
    }


    // --- Example Inputs --- ///


    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "40",
            Input: """
            162,817,812
            57,618,57
            906,360,560
            592,479,940
            352,342,300
            466,668,158
            542,29,236
            431,825,988
            739,650,466
            52,470,668
            216,146,977
            819,987,18
            117,168,530
            805,96,715
            346,949,466
            970,615,88
            941,993,340
            862,61,35
            984,92,344
            425,690,689
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "",
            Input: PartOneExamples[0].Input
        ),
    ];
}