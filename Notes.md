I ran into issues with not having all the weapons we needed from the API I was using. I added a custom weapon component that can work for what I'd hope for, but I want all the data. With the real values, without errors. I also don't want to manually copy values. So, the question is: can we read the data from the game's files to just pull the raw data into structures we want?

Or, maybe more accurately, can we find the people who've done this work before us and tweak their tools to our use case?
I found:
- [MHWNoChunk](https://www.nexusmods.com/monsterhunterworld/mods/411?tab=description) - This tool will decrypt/dechunk(? Probably more here, but it took the files from blocks of binary to usable files) the chunked files into binary files that are more usable, if you know how to read them.

  - This needed: [oo2core_8_win64.dll.html](https://www.dll-files.com/oo2core_8_win64.dll.html), which I found at the link provided.

- [MHWEditor](https://github.com/Synthlight/MHW-Editor) - This would be a tool to let you edit the game's files. I do *NOT* want to do that. I do, however, want to use the work this team did to parse the decrypted binaries into usable structs to get the data for armor, weapons, etc. in a readable form. 

  - From the notes on the page, I can filter particular tables of interest. Most are [here](https://github.com/Synthlight/MHW-Editor/wiki): 
  
    - Armor/Charms: `.am_dat`
    - Weapons: `.wp_dat` (melee) and `.wp_dat_g` (ranged)
      - I'll need to look at Weapon Abilities `wep_(wsd/wsl/glan/saxe)`... *eventually*
      - Also see Sharpness `.kire` and Shells `.shl_tbl` and Recoil `.gun_sd` and Reload `.gun_rd` and Kinsect `.rod_inse`, and maybe Coatings `.bbtbl`
    - Skills: `skill_data.skl_dat` and `skill_point_data.skl_pt_dat`, 
    - Gems: `.sgpa`
    - Mantles (maybe): `.ask` and `.asp`

There isn't a convenient way to download the data, but it will migrate fully formed via copy and paste. Not my preferred approach, but given the relatively small number of files, it will probably be faster to copy/paste into excel than to find the part of the editor code that manages the actual editor application and add functionality to spit out the data.

As of 2024-04-17, I've pulled all weapon, armor, skill, and decoration data.

I could still possibly see a use for:

- `.gun_rd`, `.gun_sd`, `.kire`, `.shl_tbl` `.rod_inse`, `.bbtbl`
- `wep_(wsd/wsl/glan/saxe)`

`.asp` contains parameters for mantle effects; how much a mantle provides resistance, how long a mantle lasts, etc. This is really not relevant to a set skill search; the only use I could possibly see might be some calculation of mantle uptime assuming optimal play that could affect an effective raw calculation, but that is well out of scope at this point.