namespace TestHagenMorphDb;

using HagenMorphDb;

internal class Program
{
    static void Main(string[] args)
    {
        Work wrk = new Work();
        wrk.Run();
    }
}

public class Work
{
    HagenMorphService _hmsrv;
         
    public Work()
    {
        var curDir = Directory.GetCurrentDirectory();
        var initStr = $"Data Source={curDir}\\Db\\HagenMorph.db;version=3;";
        _hmsrv = new HagenMorphService();//HagenMorphService(initStr, false);
    }

    private void PrintStrArr(string[] str)
    {
        Console.WriteLine("");
        for (int i = 0; i < str.Length; i++)
        {
            Console.Write($"\t{str[i]}\t");
        }
        Console.WriteLine("");
    }

    private void PrintDict(Dictionary<string, string[]> dc,PosTag pos)
    {
        string[] caseKey = ["N", "G", "D", "A", "I", "P"];
        string[] personKey = ["P1", "P2", "P3"];
        string[] type = dc["Type"];

        if (pos == PosTag.Noun || pos == PosTag.Adjective)
        {
            foreach (var key in caseKey)
            {
                var s = dc[key];
                //Console.WriteLine($"{key} == {s}");
                PrintStrArr(s);
            }
        }
        else if (pos == PosTag.Verb)
        {
            if (type[0] == "Ipf" && (type[1] == "Present" || type[1] == "Fut"))
            {
                foreach (var key in personKey)
                {
                    var s = dc[key];
                    //Console.WriteLine($"{key} == {s}");
                    PrintStrArr(s);
                }
            }
            else if(type[0] == "Ipf" && type[1] == "Pass")
            {
                var s = dc["P123"];
                //Console.WriteLine($"P123 == {s}");
                PrintStrArr(s);
            }
            else if (type[0] == "Partic")
            {
                foreach (var key in caseKey)
                {
                    var s = dc[key];
                    //Console.WriteLine($"{key} == {s}");
                    PrintStrArr(s);
                }
            }
        } 
    }   

    private void PrintFormatStringDict(FormatStringDict dict)
    {
        if(dict.Pos == PosTag.Noun)
        {
            PrintDict(dict.NounStringDict, dict.Pos);
        }
        else if(dict.Pos == PosTag.Adjective )
        {
            PrintDict(dict.AdjStringDict, dict.Pos);
        }
        else if(dict.Pos == PosTag.Verb )
        {
            PrintDict(dict.IpfPresentStrinDict,dict.Pos);
            PrintDict(dict.IpfPastStringDict,dict.Pos);
            PrintDict(dict.IpfFutureStringDict,dict.Pos);
            PrintDict(dict.IpfParticiplePresentStringDict,dict.Pos);
            PrintDict(dict.IpfParticiplePastStringDict, dict.Pos);
        }
    }

    public void Run()
    {
        Console.WriteLine("__START__");

        var res = _hmsrv.GetFormatLemmaWordPos("бегать", PosTag.Verb);

        if(res != null)
        {
            foreach (var r in res)
            {
                PrintFormatStringDict(r);
            }
        }


        Console.WriteLine("__END__");
    }
}
