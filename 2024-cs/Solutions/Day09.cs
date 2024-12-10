namespace _2024_cs.Solutions;


// --- Day 9: Disk Fragmenter --- //


public record Day09() : Solver(AnswerOne: "6356833654075", AnswerTwo: "6389911791746")
{
    public override string PartOne(IEnumerable<string> input)
    {
        var diskMap = input.First().Select(c => c - '0').ToList();
        var blockPosition = diskMap[0];
        var checksum = 0UL;
        
        var i = 1;
        var j = diskMap.Count - 1;
        
        var freeSpace = diskMap[i];
        var data = diskMap[j];

        while (i < j)
        {
            var fileId = j / 2;
            var dataToMove = Math.Clamp(data, 0, freeSpace);
            
            data -= dataToMove;

            if (data == 0)
            {
                j -= 2;
                data = diskMap[j];
            }
            
            freeSpace -= dataToMove;
            
            checksum += CalculateChecksum(blockPosition, dataToMove, fileId);
            blockPosition += dataToMove;

            while (freeSpace == 0)
            {
                i += 2;

                if (i > j) break;
                
                checksum += CalculateChecksum(blockPosition, diskMap[i - 1], (i - 1) / 2);
                blockPosition += diskMap[i - 1];
                
                freeSpace = diskMap[i];
            }
        }
        
        if (data != 0 && data != diskMap[j])
        {
            checksum += CalculateChecksum(blockPosition, data, j / 2);
        }
        
        return checksum.ToString();
    }

    private static ulong CalculateChecksum(int index, int count, int fileId)
    {
        return Enumerable.Range(index, count).Aggregate(seed: 0UL, (a, i) => a + (ulong)i * (ulong)fileId);
    }
    
    private struct Block
    {
        public int FileId;
        public int Size;
        public int Index;
        
        public bool IsFree => FileId == -1;
        public static Block None => new Block{ FileId = -1, Size = 0, Index = 0 };
    }

    public override string PartTwo(IEnumerable<string> input)
    {
        var diskMap = input.First().Select(c => c - '0').ToList();

        var blocks = new Block[diskMap.Count];
        var blockIndex = 0;

        for (var i = 0; i < diskMap.Count; i++)
        {
            var size = diskMap[i];
            
            blocks[i] = new Block{
                FileId = i % 2 == 0 ? i / 2 : -1,
                Size = size,
                Index = blockIndex
            };

            blockIndex += size;
        }

        for (var di = blocks.Length - 1; di > 0; di--)
        {
            ref var dataBlock = ref blocks[di];
            if (dataBlock.IsFree) continue;

            for (var fi = 1; fi < di; fi++)
            {
                ref var freeBlock = ref blocks[fi];
                if (!freeBlock.IsFree) continue;
                if (freeBlock.Size < dataBlock.Size) continue;
                
                // For the calculation, no need to actually move the block
                // Updating where it is enough
                dataBlock.Index = freeBlock.Index;
                freeBlock.Index += dataBlock.Size;
                freeBlock.Size -= dataBlock.Size;

                break;
            }
        }
        
        return blocks
            .Where(b => !b.IsFree)
            .Aggregate(seed: 0UL, (a, b) => a + CalculateChecksum(b.Index, b.Size, b.FileId))
            .ToString();
    }

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "1928",
            Input: """
            2333133121414131402
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "2858",
            Input: """
            2333133121414131402
            """
        ),
    ];
}