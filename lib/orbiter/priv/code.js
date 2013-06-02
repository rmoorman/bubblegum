
var List = {};

// Example data:
//
// List = {
//     Id: {
//         id: Id,
//         problems: {
//             "a": {accepted: 1, tries: 1, penalty: 1890},
//         },
//         results: {
//             n: 2,
//             penalty: 1890
//             "a": "+"
//         }
//     }
// }

// Item = {
//      who: Id,
//      problem: "a",
//      verdict: {accepted: 1, time: 134, memory: 134},
//      penalty: 1890
// }

function __MyAdd(Item) 
{
    var who = Item.who
    var p = Item.problem

    if (p in List[who].problems
        && List[who].problems[p].accepted != 0)
        return;

    var tries = (p in List[who].problems ? List[who].problems[p].tries : 0)
    tries++
    List[who].problems[p] = {
        penalty: Item.penalty,
        accepted: Item.verdict.accepted,
        tries: tries
    }

    List[who].results[p] = (Item.verdict.accepted ? "+" : ("-" + tries))
    if (Item.verdict.accepted)
    {
        List[who].results["n"]++
        List[who].results["penalty"] += 20 * (tries - 1) + Item.penalty
    }
}

function ncmp(a, b) // number compare
{
    if (a < b)
        return -1;
    if (a > b)
        return 1;
    return 0;

function __MyCmp(a, b)
{
    if (a.results["n"] == b.results["n"])
        return ncmp(a.results["penalty"], b.results["penalty"])
    return ncmp(a.results["n"], b.results["n"])
}

function __Add(items)
{
    var addF = (add ? add : __MyAdd);
    var cmpF = (cmp ? cmp : __MyCmp);
    for (var i in items) 
    {
        var who = items[i].who
        if (!(who in List))
            List[who] = {id: who, problems: {}, results: {"n": 0, "penalty": 0}}
        addF(items[i])
    }
    var table = new Array()
    //table.length = Object.keys(List).length
    var i = 0
    for (var it in List)
        table[i++] = List[it]
    
    table.sort(cmpF)

    return table;
}

