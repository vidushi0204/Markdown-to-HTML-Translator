fun mdt2html(filename:string)=
    let
        val inputf=TextIO.openIn(filename^".mdt");
        val outputf=TextIO.openOut(filename^".html");
        fun underline_help( l : char list, check : bool)=
            if null l
            then 
                ([],false)
            else if hd l = chr(92) andalso length(l) >= 2
            then
                let
                    val tp = underline_help(tl(tl(l)),check)
                in
                    ([hd l, hd(tl(l))] @ #1tp,#2tp)
                end
            else if hd l = #"_"
            then
                if check
                then
                    let
                        val tp=underline_help(tl l,true)
                    in
                        if #2tp 
                        then
                            ( [#" "] @ #1tp,true)
                        else
                            ( [#"<",#"/",#"u",#">",#" "] @ #1tp,true) 
                        end
                else
                    let
                        val tp=underline_help(tl l,true)
                    in
                        if #2tp 
                        then
                            ( [#" "] @ [#"<",#"u",#">"] @ #1tp, false)
                        else
                            ( [hd l] @ #1tp, false) 
                    end
            else if hd l = #" "
            then
                let
                    val tp = underline_help(tl l,false)
                in
                    ([hd l] @ #1tp,false)
                end

            else
                let
                    val tp = underline_help(tl l,check)
                in
                    ([hd l] @ #1tp,#2tp)
                end;  

        fun underline( l : char list)=
            let
                val tp = underline_help(l,false) 
            in 
                #1 tp
            end;
        fun hashcnter(l:char list, count: int)=
            if length(l)= 0 orelse hd(l) <> #"#" orelse count = 6
            then
                (l,count)
            else
                hashcnter(tl(l),count+1) ;  
        fun whitespace(line: char list)=
            if length(line)>0 andalso hd(line) = #" " 
            then
                whitespace(tl(line))
            else
                line; 



        fun headings(l: char list)=
            let
                val tup = hashcnter(l,0)
            in
                if length(whitespace(l)) = 1
                then
                    explode("</p><p>")
                else if #2tup > 0
                then
                    [#"<",#"h",chr(#2tup+48),#">"]@(#1tup)@[#"<",#"/",#"h",chr(#2tup+48),#">"] 
                else
                    l
            end;
        fun boldctrl(prefix:char list, lun: char list)=
            if length(lun)=0
            then
                (prefix,[])
            else if hd(lun) = #"*" andalso length(tl(lun))>0 andalso hd(tl(lun)) = #"*"
            then
                ([#"<",#"b",#">"]@(tl(tl(prefix)))@[#"<",#"/",#"b",#">"],tl(tl(lun)))
            else
                boldctrl(prefix@[hd(lun)],tl(lun));

        fun bold(line: char list)=
            if length(line)=0
            then
                []
            else if hd(line) = chr(92) andalso length(line)>1 
            then
                [hd(line)]@[hd(tl(line))]@bold(tl(tl(line)))
            else if hd(line) = #"*" andalso length(tl(line))>0 andalso hd(tl(line)) = #"*"
            then 
                let 
                    val q= boldctrl([#"*",#"*"],tl(tl(line))) 
                in
                    (#1 q)@bold(#2 q)
                end
            else
                [hd(line)]@bold(tl(line));
                
        fun italctrl(prefix:char list, lun: char list)=
            if length(lun)=0
            then
                (prefix,[])
            else if hd(lun) = #"*"
            then
                ([#"<",#"e",#"m",#">"]@(tl(prefix))@[#"<",#"/",#"e",#"m",#">"],tl(lun))
            else
                italctrl(prefix@[hd(lun)],tl(lun));
        fun italicize(line: char list)=
            if length(line)=0
            then
                []
            else if hd(line) = chr(92) andalso length(line)>1 
            then
                [hd(line)]@[hd(tl(line))]@italicize(tl(tl(line)))
            else if hd(line) = #"*"
            then 
                let 
                    val q= italctrl([#"*"],tl(line))
                in
                    (#1 q)@italicize(#2 q)
                end
            else
                [hd(line)]@italicize(tl(line));

        (* Design decision: **** --> <b></b> and remaining stray ** --> <em></em> *)
        fun horicnter(prefix: char list, l:char list, cnt: int)=
            if length(l)=0 
            then
                if cnt>2
                then    
                    prefix@[#"<",#"h",#"r",#">"]
                else if cnt = 2
                then
                    prefix@[#"-",#"-"]
                else if cnt = 1
                then
                    prefix@[#"-"]
                else
                    prefix
            else if hd(l) <> #"-" 
            then
                if hd(l) = chr(92) andalso length(l)>=2
                then
                    horicnter(prefix,[],cnt)@horicnter([hd(l),hd(tl(l))],tl(tl(l)),0)
                else
                    horicnter(prefix,[],cnt)@horicnter([hd(l)],tl(l),0)
            else
                horicnter(prefix,tl(l),cnt+1);
            
        fun horizontal_line(l:char list)=
            if length(l)>0
            then
                horicnter([],l,0)
            else
                [];

        fun tabline(prefix: char list,line: char list)=
            if length(line)=0
            then
                prefix
            else if hd(line) = #"|"
            then
                tabline(prefix@explode("</td><td>"),tl(line))
            else
                tabline(prefix@[hd(line)],tl(line));

        (* <<< --> still table *)
        

        fun blockcount(l:char list,count:int)=
            if length(whitespace(l))>0 andalso hd(whitespace(l)) = #">"
            then    
                blockcount(tl(l),count+1)
            else    
                (l,count);
        fun blockinsert(l:char list,count: int)=
            if count = 0
            then
                l
            else 
                blockinsert(explode("<blockquote>")@l@explode("</blockquote>"),count-1);
        fun blockquote(l:char list)=
            let
                val line = blockcount(l,0)
            in
                blockinsert(#1line,#2line)
            end;
        fun autoctrl(prefix: char list,lun :char list)=
            if length(lun)=0
            then
                (prefix,[])
            else if hd(lun) = #">"
            then
                (explode("<a href='http")@prefix@explode("'> http")@prefix@explode("</a>"),tl(lun))
            else
                autoctrl(prefix@[hd(lun)],tl(lun));
        fun autolink(l:char list)=
            if length(l)=0
            then
                []
            else if hd(l) = #"<" andalso length(l)>=6 andalso hd(tl(l)) = #"h" andalso hd(tl(tl(l))) = #"t" andalso hd(tl(tl(tl(l)))) = #"t" andalso hd(tl(tl(tl(tl(l))))) = #"p"
            then 
                let 
                    val pew= autoctrl([],tl(tl(tl(tl(tl(l))))))
                in
                    (#1 pew)@autolink(#2 pew)
                end
            else
                [hd(l)]@autolink(tl(l));

        fun hardtext(prefix:char list, l: char list)=
            if length(l)=0
            then
                (prefix,[])
            else if hd(l) = #"]"
            then
                (prefix,tl(l))       
            else
                hardtext(prefix@[hd(l)],tl(l));
        fun hardctrl(t:char list,prefix: char list,lun :char list)=
            if length(lun)=0
            then
                (prefix,[])
            else if hd(lun) = #")"
            then
                (explode("<a href='http")@prefix@explode("'>")@t@explode("</a>"),tl(lun))
            else
                hardctrl(t,prefix@[hd(lun)],tl(lun));
        fun hardlink(l:char list)=
            if length(l)=0
            then
                []
            else if hd(l) = #"["
            then
                let 
                    val pew = hardtext([],tl(l))
                    val s = #2 pew
                    val t = #1 pew
                in
                    if hd(s) = #"(" andalso length(s)>=6 andalso hd(tl(s)) = #"h" andalso hd(tl(tl(s))) = #"t" andalso hd(tl(tl(tl(s)))) = #"t" andalso hd(tl(tl(tl(tl(s))))) = #"p"
                    then
                        let 
                            val pew= hardctrl(t,[],tl(tl(tl(tl(tl(s))))))
                        in
                            (#1 pew)@autolink(#2 pew)
                        end                
                    else
                        [#"["]@t@[#"]"]@hardlink(s)
                end
            else
                [hd(l)]@hardlink(tl(l));

        fun unordered_list(l: char list)=
            let 
                val f= whitespace(l)
            in
                if length(f)>=2 andalso hd(f) = #"-" andalso hd(tl(f)) = #" " 
                then
                    explode("<li>")@tl(tl(f))
                else
                    l
            end

        fun cnt_whitespace(line: char list,count: int)=
            if length(line)>0 andalso hd(line) = #" " andalso count <8
            then
                cnt_whitespace(tl(line),count+1)
            else 
                (line,count); 
        
        fun tabend(l: char list)=
            if length(l)=0
            then
                false
            else if length(l)>1 andalso hd(l) = #">" andalso hd(tl(l)) = #">" 
            then
                true
            else
                tabend(tl(l));
        fun tabreader()=
            let
                val line = whitespace(explode(valOf(TextIO.inputLine(inputf))))
            in  
                if tabend(line)
                then
                    []
                else
                    horizontal_line(underline(italicize(bold(hardlink(autolink(explode("<tr><td>")@tabline([],line)@explode("</td></tr>")@tabreader()))))))
            end;
        fun table(l:char list)=
            if length(l)=0
            then
                []
            else if length(l)>1 andalso hd(l) = #"<" andalso hd(tl(l)) = #"<" 
            then
                explode("<center><table border='1'>")@tabreader()@explode("</table></center>")
            else
                [hd(l)]@table(tl(l));

        fun sus(l: char list)=
            if length(l)=0
            then
                []
            else if hd(l) = #"<"
            then
                explode("&lt;")@sus(tl(l))
            else if hd(l) = #">"
            then
                explode("&gt;")@sus(tl(l))
            else
                [hd(l)]@sus(tl(l));
        fun gawd(line: string)=
            let
                val lone=if String.size(line) = 0 then [] else explode(line)
                val ion= cnt_whitespace(lone,0)
            in
                if (#2 ion) >=8
                then
                    implode(headings(explode("<pre><code>")@sus(lone)@explode("</code></pre>")))
                else
                    implode(horizontal_line(underline(italicize(bold(headings(blockquote(hardlink(autolink(table(unordered_list(lone)))))))))))
            end;
            
        fun scanl()=
            let
                val eofe=TextIO.endOfStream(inputf)
                val line=if eofe then "" else valOf(TextIO.inputLine(inputf))     
                val ol=gawd(line)
            in
                if eofe then "" else ol^scanl()
            end;
    in   
        TextIO.output(outputf,"<p>"^scanl()^"</p>");
        TextIO.closeOut(outputf)
    end;
