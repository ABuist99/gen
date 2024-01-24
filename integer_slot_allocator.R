#given a target integer and a vector of integers, find the smallest minimum sum needed
    #to equal or exceed target 
    slotty = function(n, slots){
        slots = sort(unique(slots))

        prev_c = Inf
        prev_n = Inf
        best_out = NA
        for(i in length(slots):1){
            slot_tmp = slots[1:i]
            n_tmp = n

            out = rep(0,length(slots))
            names(out) = slots

            while(n_tmp > 0){
                if(length(slot_tmp) > 1){
                    while(abs(n_tmp - max(slot_tmp)) + tail(head(slot_tmp, length(slot_tmp)-1),1) >= max(slot_tmp) & n_tmp < max(slot_tmp)){
                        slot_tmp = head(slot_tmp, length(slot_tmp)-1)
                        if(length(slot_tmp) == 1){break}
                    }
                }
                n_tmp = n_tmp - max(slot_tmp)
                out[which(slots == max(slot_tmp))] = out[which(slots == max(slot_tmp))] + 1
            }
            cur_c = sum(out)
            cur_n = max(as.numeric(names(out[which(out > 0)])))

            if(prev_c >= cur_c &
               prev_n >= cur_n){
                prev_c = cur_c
                prev_n = cur_n
                best_out = out
            }
        }
        return(best_out)
    }
