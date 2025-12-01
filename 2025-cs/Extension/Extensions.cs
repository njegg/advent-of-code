namespace _2024_cs.Extension;

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
    
    public static IEnumerable<(T e, int i)> WithIndex<T>(this IEnumerable<T> source)
    {
        return source.Select((item, index) => (item, index));
    }
    
    public static string Stringify<T>(this IEnumerable<T> source, string by = ", ")
    {
        return string.Join(by, source);
    }
}
