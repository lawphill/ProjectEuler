#! usr/bin/perl

$filename = "data/names.txt";
open($fh,"<",$filename)or die("couldn't open $filename: $!\n");
@data = <$fh>;
close($fh);

@names = split(",",$data[0]);
@names = sort(@names);
$total = 0;


%alpha_nums;
$number = 1;
foreach('A'..'Z'){
	$alpha_nums{$_} = $number;
	$number++;
}

print "$names[$#names]\n";
for $i (1..$#names+1){
	@char = split("",$names[$i-1]);
	foreach (@char){
		$total += $alpha_nums{$_} * $i;
	}
}

print "Total: $total\n";
