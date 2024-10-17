namespace SetSearch

module Helpers = 
    open Interfaces
    
    
    /// <summary>
    /// Compares two skills, potentially of different types, by Id alone.
    /// </summary>
    let areSameSkill (this: 'a when Skill<'a>) (that: 'b when Skill<'b>) = this.SkillId = that.SkillId

    let asCounts (xs: 'a seq) = xs |> Seq.countBy id |> List.ofSeq

    let asItems (xs: ('a * int) seq) = [
        for x, count in xs do
            for i in 1..count -> x
    ]