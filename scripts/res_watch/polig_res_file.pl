#!/usr/bin/perl -w

# файл ищет все файлы в папке и ...
# в итоге составляет файл 

use strict;

# имя папки для сканирования по умолчанию (. = текущая папка)
my $dir = "/home/scas/devol/res";
# отсечения для формирования относительного пути для нужного домена
my $drop_patch =  "/home/scas/devol/";

# берем имя папки, если задано аргументом командной строки
$dir = $ARGV[0] if defined $ARGV[0];
$drop_patch = $ARGV[1] if defined $ARGV[1];
# вызов процедуры сканирования содержимого папки
my %list = scan_dir ( $dir );

# вызов процедуры сохранения массива в файл
&save_list ( %list );

# --------------------------------------------------------------------
sub scan_dir
# процедура рекурсивного сканирования папки
{

    my ($dir) = @_;
    my %list;
    # aka find /patch/ -type f
    for my $file (glob($dir.'/*'))
    {

        if (-d $file) # если файл является папкой
        {
	#  некоторые подпапки отрезаем  - Нет решено держать эталонные папки ресурсов держать чистыми
#	if($file =~ // ) {
            my %sub = scan_dir($file);
	# %a = (%a, %b); 
	%list = (%list, %sub);
#	}

        } elsif(bool_is_valid_file($file)) { 
	# файл не папка а видимо просто файл  причем валидный файл не всякий 

	my $crc = `/usr/bin/md5sum $file`;
	my @A=split(' ', $crc);
#	print $file," ",$A[0],"\n";
	$file =~ s/$drop_patch//g;
	$list{$file}=$A[0];
#	print "   file add: ",$file, "\n";
	#push @list, $file;
	}

    }
    return %list;

}
#---------------------------------------------------------------------
sub bool_is_valid_file {
	my ($file) = @_;
	    if($file =~ /Thumbs.db/ ) {
	return 0;
	}
	return 1;
}

# --------------------------------------------------------------------
sub save_list
# процедура сохранения списка в файл
{
    my (%list) = @_;
#    open(F, '>file_list.txt');
#    print F join("\n", @list) . "\n";
#    close(F);
## надо отдавать что то вроде URL d58e40a5b9d42bced2cb2fbb00128f34
#my $pr_print=0;
while(my ($k,$v)=each(%list))
{	
	print $k,' ',$v,"\n";
}
#print " ok.", "\n";
}

# --------------------------------------------------------------------
__END__ 
