#include <stdio.h>

/*to be updated and profiled*/

/*You can compress some values if you know that the possible ranges they can have*/
struct PlayerField
{
  unsigned char male : 1;   /*1 for male, 0 for female*/
  unsigned char wpn : 2;    /*0-3 for weapon choice*/
  unsigned char ammo : 4;   /*Ammo - maximum 2^4-1*/
  unsigned char unused : 1; /*Unused; recommended for debugging spillovers*/
};

/*
Warning: http://en.wikipedia.org/wiki/Bit_field
First, the ordering of bits in memory is cpu dependent and memory padding rules can vary between compilers.
In addition, less well optimized compilers sometimes generate poor quality code for reading and writing bit members 
and there are potentially thread safety issues relating to bit fields because most machines cannot manipulate arbitrary 
sets of bits in memory, but must instead load and store whole words.

G++ compiler does this:
MSB                 LSB
|unused|ammo|wpn|male|

See encode_value for an alternative
*/
struct PlayerInfo
{
  union
  {
    PlayerField field;  
    unsigned char byte;
  };
  
  PlayerInfo()
    :byte(0)
    { 
      field.male = 0;
      field.wpn = 0;
      field.ammo = 0;
      field.unused = 0;
    }
    
  void operator=(unsigned char mychar)
  {
    byte = mychar;
  }
  
  unsigned char GetEncodedValue()
  {
    return byte;
  }
  

  /*
  In this function, we are rearranging the bits to:
  MSB                 LSB
  |unused|ammo|wpn|male|
  8      7    3   1    0
  
  There are also safety checks in this function as well.
  - (fields.myfield & <maximum value the field can have>) : Using logic &, we can make sure the value is within the maximum value the value can store.
  - << <position in the byte> : Left shift it to the position you want in the byte(s). Take note of the size here. Draw a diagram to help you if possible.
  - result |= .. : Add the bits to result
  */
  unsigned char encode_value(PlayerField fields)
  {
    unsigned char result = 0;
    result |= (fields.male & 0x01) << 0;
    result |= (fields.wpn & 0x03)  << 1;
    result |= (fields.ammo & 0x0F) << 3;
    result |= (fields.unused & 0x01) << 7;
    return result;
  }
  
  /*
  To decode the values, we shift it to the right and do a logic &
  (val >> start position) & (maximum value of the field)
  */
  void decode_value(unsigned char val)
  {
    PlayerField fields;
    fields.male = val & 0x01;
    fields.wpn = (val >> 1) & 0x03;
    fields.ammo = (val >> 3) & 0x0F;
    fields.unused = (val >> 7) & 0x01;
    
    printf("Decoded Male: %d\n", fields.male);
    printf("Decoded Weapon choice: %d\n", fields.wpn);
    printf("Decoded Ammo: %d\n", fields.ammo);
    printf("Decoded Unused: %d\n", fields.unused);
  }

};

int main()
{
  PlayerInfo myplayer;

  unsigned char valuereadfromfile = 255; /*Assume value read from file*/
  myplayer = valuereadfromfile;
  printf("Value read: %d\n", valuereadfromfile);
  
  printf("Male: %d\n", myplayer.field.male);
  printf("Weapon choice: %d\n", myplayer.field.wpn);
  printf("Ammo: %d\n", myplayer.field.ammo);
  printf("Unused: %d\n", myplayer.field.unused);

  myplayer.field.unused = 0;
  myplayer.field.male = 1;
  myplayer.field.wpn = 3;
  myplayer.field.ammo = 1;
  unsigned char valuetowrite = myplayer.GetEncodedValue();
  printf("\nChanged value to write: %d\n", valuetowrite);
  printf("Male: %d\n", myplayer.field.male);
  printf("Weapon choice: %d\n", myplayer.field.wpn);
  printf("Ammo: %d\n", myplayer.field.ammo);
  printf("Unused: %d\n\n", myplayer.field.unused);

  PlayerField f;
  f.male = 1;
  f.wpn = 3;
  f.ammo = 1;
  f.unused = 0;
  unsigned char val = myplayer.encode_value(f);
  printf("Encoded value: %d\n", val);
  myplayer.decode_value(val);
  
  return 0;
}
