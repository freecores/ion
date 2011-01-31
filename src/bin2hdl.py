""" xcxcxc


"""
import sys
import getopt
import math


def usage():
    print "usage:"
    print "python bin2hdl.py [arguments]\n"
    print "Inserts data in VHDL template\n"
    print "ALL of the following arguments should be given, in any order:"
    print "{c|code} <filename>        Code binary image file name"
    print "{v|vhdl} <filename>        VHDL template"
    print "{a|architecture} <name>    Name of target VHDL architecture"
    print "{e|entity} <name>          Name of target VHDL entity"
    print "{o|output} <filename>      Target VHDL file name"
    print "code_size <number>         Size of code memory in words (decimal)"
    print "data_size <number>         Size of data memory in words (decimal)"
    print ""
    print "Additionally, any of these arguments can be given:"
    print "{s|sim_len} <number>       Length of simulation in clock cycles"
    print "{d|data} <filename>        Data binary image file name"
    print "{h|help}                   Display some help text and exit"
    print "{i|indent} <number>        Indentation in VHDL tables (decimal)"

def help():
    print "\nPurpose:\n"
    print "Reads the code and data binary files and 'slices' them in byte"
    print "columns."
    print "The data columns are converted to VHDL strings and then inserted"
    print "into the vhdl template, in place of tags @code0@ .. @code3@ and "
    print "@data0@ .. @data3@. Column 0 is LSB and column3 is MSB.\n"
    print "Other template tags are replaced as follows:"
    print "@entity_name@         : Name of entity in target vhdl file"
    print "@arch_name@           : Name of architecture in target vhdl file"
    print "@sim_len@             : Length of simulation in clock cycles"
    print "@code_table_size@     : Size of code RAM block, in words"
    print "@code_addr_size@      : ceil(Log2(@code_table_size@))"
    print "@data_table_size@     : Size of data RAM block, in words"
    print "@data_addr_size@      : ceil(Log2(@data_table_size@))"
    

def build_vhdl_tables(code,table_size, indent_size):
    # Build the four byte column tables. [0] is LSB, [3] is MSB
    tables = [[0 for i in range(table_size)] for i in range(4)]

    # Separate binary data into byte columns
    # (here's where data endianess matters, we're assuming big endian)
    byte = 0    # byte 0 is LSB, 3 is MSB
    index = 0   # index into column table    
    for c in code:
        #print str(ord(c)) + " " +  str(byte) + " " + str(index)
        tables[3-byte][index] = ord(c)
        #for k in tables:
        #    print k[0:4]
        byte = byte + 1
        if byte == 4:
            byte = 0
            index = index + 1
    
    # Write the data for each of the four column tables as a VHDL byte
    # constant table.
    vhdl_data_strings = [" "*indent_size]*4
    
    for j in range(4):
        col = 0
        word = len(tables[j])
        for c in tables[j]:
            word = word - 1
            if word > 0:
                item = "X\"%02X\"," % c
            else:
                item = "X\"%02X\"" % c
            col = col + 1
            if col == 8:
                col = 0
                item = item + "\n" + " "*indent_size
            vhdl_data_strings[j] = vhdl_data_strings[j] + item

    return vhdl_data_strings
    
def main(argv):
    code_filename = ""          # file with code sections (text+reginfo+rodata)
    data_filename = ""          # file with data sections (data+bss)
    vhdl_filename = ""          # name of vhdl template file
    entity_name = "mips_tb"     # name of vhdl entity to be generated
    arch_name = "testbench"     # name of vhdl architecture to be generated
    target_filename = "tb.vhdl" # name of target vhdl file
    indent = 4                  # indentation for table data, in spaces
    code_table_size = -1        # size of VHDL table
    data_table_size = -1        # size of VHDL table
    bin_words = 0               # size of binary file in 32-bit words 
    simulation_length = 22000   # length of logic simulation in clock cycles
    
    #

    try:                                
        opts, args = getopt.getopt(argv, "hc:d:v:a:e:o:i:s:", 
        ["help", "code=", "data=", "vhdl=", "architecture=", 
         "entity=", "output=", "indent=", "sim_len=",
         "code_size=", "data_size="])
    except getopt.GetoptError:
        usage()
        sys.exit(2)  

    # Parse coommand line parameters
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            usage()
            help()
            exit(1)
        if opt in ("-v", "--vhdl"):
            vhdl_filename = arg
        elif opt in ("-o", "--output"):
            target_filename = arg
        elif opt in ("-c", "--code"):
            code_filename = arg
        elif opt in ("-d", "--data"):
            data_filename = arg
        elif opt in ("-a", "--architecture"):
            arch_name = arg
        elif opt in ("-e", "--entity"):
            entity_name = arg
        elif opt in ("-i", "--indent"):
            indent = int(arg)
        elif opt in ("-s", "--sim_len"):
            simulation_length = int(arg)
        elif opt == "--code_size":
            code_table_size = int(arg)
        elif opt == "--data_size":
            data_table_size = int(arg)
    
    # See if all mandatory options are there
    if code_filename=="" or vhdl_filename=="" or \
       code_table_size < 0 or data_table_size<0:
        usage()
        sys.exit(2)

        
    # Open binary code and data input files and read them into buffers
    try:
        fin = open(code_filename, "rb")
        code = fin.read()
        fin.close()
    except IOError:
        print "Binary File %s not found" % code_filename

    if data_filename != "":
        try:
            fin = open(data_filename, "rb")
            data = fin.read()
            fin.close()
        except IOError:
            print "Binary File %s not found" % data_filename
        
    #print "Read " + str(len(code)) + " bytes."
    
    # Make sure the code and data will fit in the tables
    bin_words = len(code) / 4
    if bin_words > code_table_size:
        print "Code does not fit table: " + str(bin_words) + " words,",
        print str(code_table_size) + " table entries"
        sys.exit(1)
    
    if data_filename != "":
        # FIXME We're not checking for BSS size here, only .data (?)
        bin_words = len(data) / 4
        if bin_words > data_table_size:
            print "Data does not fit table: " + str(bin_words) + " words,",
            print str(data_table_size) + " table entries"
            sys.exit(1)


    # Build the VHDL strings for each slice of both code and data tables
    vhdl_code_strings = build_vhdl_tables(code, code_table_size, indent)
    if data_filename != "":
        vhdl_data_strings = build_vhdl_tables(data, data_table_size, indent)
    else:
        # In case we didn't get a data binary, we want the vhdl compilation 
        # to fail when @data@ tags are used, just to catch the error
        vhdl_data_strings = ["error: missing data binary file"]*4
    
    # Now start scanning the VHDL template, inserting data where needed
    
    # Read template file...
    fin = open(vhdl_filename, "r")
    vhdl_lines = fin.readlines()
    fin.close()        
    
    # ...and build the keyword and replacement tables
    keywords = ["@code0@","@code1@","@code2@","@code3@",
                "@data0@","@data1@","@data2@","@data3@",
                "@entity_name@","@arch_name@",
                "@sim_len@",
                "@code_table_size@","@code_addr_size@",
                "@data_table_size@","@data_addr_size@"];
    replacement = vhdl_code_strings + vhdl_data_strings + \
                 [entity_name, arch_name, 
                  str(simulation_length),
                  str(code_table_size), 
                  str(int(math.floor(math.log(code_table_size,2)))),
                  str(data_table_size), 
                  str(int(math.floor(math.log(data_table_size,2))))]
    
    # Now traverse the template lines replacing any keywords with the proper 
    # vhdl stuff we just built above.
    output = ""
    for vhdl_line in vhdl_lines:
        temp = vhdl_line
        for i in range(len(keywords)):
            if temp.rfind(keywords[i]) >= 0:
                temp = temp.replace(keywords[i], replacement[i])
                # uncomment this break to check for ONE keyword per line only
                #break
        output = output + temp
    
    try:
        fout = open(target_filename, "w")
        fout.write(output)
        fout.close()
        print "Wrote VHDL file '%s'" % target_filename
    except IOError:
        print "Could not write to file %s" % target_filename
    
    
    sys.exit(0)
        


if __name__ == "__main__":
    main(sys.argv[1:])

    sys.exit(0)

