using Xunit;

namespace _2025_cs.Extension;

public static class Extensions
{
    public static List<T> With<T>(this List<T> source, T value)
    {
        source.Add(value);
        return source;
    }

    public static IEnumerable<R> Select<T, K, R>(this IEnumerable<(T, K)> source, Func<T, K, R> tup)
    {
        return source.Select(t => tup(t.Item1, t.Item2));
    }

    public static void Mutate<T>(this T[] source, Func<T, T> projection)
    {
        for (var i = 0; i < source.Length; i++)
        {
            source[i] = projection(source[i]);
        }
    }

    public static IEnumerable<T> RepeatForever<T>(this IEnumerable<T> sequence)
    {
        var enumerable = sequence as T[] ?? sequence.ToArray();
        while (true)
            foreach (var item in enumerable)
                yield return item;
    }

    public static (T Max, int Index) Max<T>(this T[] sequence) where T : IComparable
    {
        if (sequence.Length == 0) throw new ArgumentException("Sequence contains no elements");

        T? max = default;
        var index = -1;

        for (var i = 0; i < sequence.Length; i++)
        {
            if (sequence[i].CompareTo(max) > 0)
            {
                max = sequence[i];
                index = i;
            }
        }

        return (max!, index);
    }

    public static IEnumerable<(T e, int i)> WithIndex<T>(this IEnumerable<T> source)
    {
        return source.Select((item, index) => (item, index));
    }

    public static string Stringify<T>(this IEnumerable<T> source, string by = ", ")
    {
        return string.Join(by, source);
    }

    public static IEnumerable<T> Peek<T>(this IEnumerable<T> source, Action<T> peekFn)
    {
        foreach (var el in source) peekFn(el);

        return source;
    }

    public static IEnumerable<T> WhereNot<T>(this IEnumerable<T> source, Func<T, bool> predicate)
    {
        return source.Where(x => !predicate(x));
    }

    public static List<List<T>> SplitOn<T>(this IEnumerable<T> source, T on) where T : notnull
    {
        List<List<T>> result = [];
        List<T> current = [];

        foreach (var el in source)
        {
            if (el.Equals(on))
            {
                if (current.Any())
                {
                    result.Add(current);
                    current = [];
                }
            }
            else
            {
                current.Add(el);
            }
        }

        if (current.Any())
        {
            result.Add(current);
            current = [];
        }

        return result;
    }

    public static List<List<T>> SplitOn<T>(this IEnumerable<T> source, Func<T, bool> predicate) where T : notnull
    {
        List<List<T>> result = [];
        List<T> current = [];

        foreach (var el in source)
        {
            if (predicate(el))
            {
                if (current.Any())
                {
                    result.Add(current);
                    current = [];
                }
            }
            else
            {
                current.Add(el);
            }
        }

        if (current.Any())
        {
            result.Add(current);
            current = [];
        }

        return result;
    }

    public static T[][] CWRotate<T>(this T[][] matrix)
    {
        var height = matrix.Count();
        var width = matrix.First().Count();

        T[][] transposedMatrix = new T[width][];
        for (var i = 0; i < width; i++) transposedMatrix[i] = new T[height];

        for (var y = 0; y < height; y++)
        {
            for (var x = 0; x < width; x++)
            {
                transposedMatrix[x][height - 1 - y] = matrix.ElementAt(y).ElementAt(x);
            }
        }

        return transposedMatrix;
    }

    public static T[][] CCWRotate<T>(this T[][] matrix)
    {
        var height = matrix.Count();
        var width = matrix.First().Count();

        T[][] transposedMatrix = new T[width][];
        for (var i = 0; i < width; i++) transposedMatrix[i] = new T[height];

        for (var y = 0; y < height; y++)
        {
            for (var x = 0; x < width; x++)
            {
                transposedMatrix[width - 1 - x][y] = matrix.ElementAt(y).ElementAt(x);
            }
        }

        return transposedMatrix;
    }
}

public class ExtensionTests
{
    [Fact]
    public void CWRotateTests()
    {
        int[][] m = [
            [1, 2, 3, 4],
            [5, 6, 7, 8],
            [9, 10, 11, 12],
        ];

        int[][] rotated = [
            [9, 5, 1],
            [10, 6, 2],
            [11, 7, 3],
            [12, 8, 4]
        ];

        Assert.Equal(expected: rotated, actual: m.CWRotate());
    }

    [Fact]
    public void CCWRotateTests()
    {
        int[][] m = [
            [1, 2, 3, 4],
            [5, 6, 7, 8],
            [9, 10, 11, 12],
        ];

        int[][] rotated = [
            [4, 8, 12],
            [3, 7, 11],
            [2, 6, 10],
            [1, 5, 9]
        ];

        Assert.Equal(expected: rotated, actual: m.CCWRotate());
    }

    [Fact]
    public void SplitOnTests()
    {
        int[] actual = [1, 2, 3, 4, 5, 6, 7, 8];

        Assert.Equal(expected: [[1, 2, 3, 4], [6, 7, 8]], actual: actual.SplitOn(5));
    }
}
