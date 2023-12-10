namespace AoC_2023.Extension;

public static class Extensions
{
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
    
    public static IEnumerable<(T e, int i)> WithIndex<T>(this IEnumerable<T> source)
    {
        return source.Select((item, index) => (item, index));
    }
}