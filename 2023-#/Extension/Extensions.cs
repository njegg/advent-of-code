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
}