
#ifndef _HASHTABLE_STR_H
#define _HASHTABLE_STR_H

#include "Prerequisites.h"
#include <vector>

DECLARE_ENGINE_NAMESPACE
/** \ingroup ResourceManagement
 * @{
 */

/**
 * \class HashItemStr
 *
 * Used internally to keep track of key-value pairs in the hashtable.
 * Strings are used as the keys
 */
template <typename Object>
class HashItemStr
{
	public:
		HashItemStr() : m_key(""), m_obj(0) {}
		~HashItemStr() {}

		String& GetKey()
		{
			return m_key;
		}

		void SetKey(const String &k)
		{
			m_key = k;
		}

		Object* GetObject()
		{
			return m_obj;
		}

		void SetObj(Object* obj)
		{
			m_obj = obj;
		}

		bool operator==(HashItemStr &item)
		{
			if(m_key == item.GetKey())
				return true;
			return false;
		}

		void operator=(HashItemStr item)
		{
			m_key = item.GetKey();
			m_obj = item.GetObject();
		}

	private:
		String m_key;
		Object *m_obj;
};


/**
 * \class HashTableStr
 *
 * A HashTable implementation, using strings as keys.
 * Warning: Hash function is inefficient(lots of collisions), maybe use md5 to replace it?
 * TODO: Switch from using std::vectors to using a more simplified version?(only needs size and push_back/pop functionality)
 */
template <typename A>
class HashTableStr
{
	public:
		/**
		 * Constructor
		 * \param size			The minimum number of hashes supported
		 */
		HashTableStr(int32_t size) : m_size(0)
		{
			uint32_t prime_table[10] = { 211, 509, 1009, 5009, 10009, 20011, 50021, 100003, 200003, 500009};

			int32_t index = 0;
			while (index < 8)
			{
				m_size = prime_table[index];
				if (size <= m_size)
					break;
				++index;
			}
			m_table = new std::vector< HashItemStr<A> >[m_size];
		}

		/**
		 * Deletes the hash table. Use Clear() if you want to delete the elements in the hash table.
		 */
		~HashTableStr()
		{
			if (m_table)
			{
				delete[] m_table;
				m_table = 0;
			}
			m_size = 0;
		}

		/**
		 * Use clear to delete the elements inside the vector
		 */
		void Clear()
		{
			if (m_table)
			{
				for (int32_t i=0; i<m_size; ++i)
				{
					for (int32_t a=0; a<m_table[i].size(); ++a)
					{
						delete m_table[i][a].GetObject();
						m_table[i].erase(m_table[i].begin+a);
					}
				}
			}
		}

		/**
		 * Add an item into the hashtable
		 */
		bool Insert(HashItemStr<A> &obj)
		{
			if (obj.GetKey() == "")
				return false;

			int32_t hash = HashFunction_1(obj.GetKey());

			std::vector<HashItemStr<A>> *ptr;

			int32_t prevhash = -1;
			int32_t size = 0;
			while (prevhash != hash)
			{
				ptr = &m_table[hash];
				size = (int32_t)ptr->size();
				if (size) //something in that slot
				{
					for (int32_t i=0; i<size; ++i)
					{
						if ( (*ptr)[i].GetKey() == obj.GetKey() ) //Key already taken
							return false; //error + item
					}
					prevhash = hash;
					hash = ReHashFunction(obj.GetKey());
				}
				else
				{
					m_table[hash].push_back(obj);
					return true; //You can insert it, your job is over now
				}
			}
			//Alternative: push back into the vector
			m_table[hash].push_back(obj);
			//std::cout << "Collision: " <<  obj.GetKey() << std::endl;
			return true;

			//exception here: unable to store item
			//RESIZE TABLE?
		}

		/**
		 * Remove a particular item from the hashtable. uses delete on the object
		 * NOTE: Do not use with memory pools containing derived classes.
		 */
		bool Delete(const String &key)
		{
			int32_t hash = HashFunction_1(key);

			std::vector< HashItemStr<A> > *ptr;

			int32_t prevhash = -1;
			int32_t size = 0;
			while (prevhash != hash)
			{
				ptr = &m_table[hash];
				size = (int32_t)ptr->size();
				for (int32_t i=0; i<size; ++i)
				{
					if ((*ptr)[i].GetKey() == key)
					{
						delete (*ptr)[i].GetObject();
						(*ptr)[i].SetObj(0);
						ptr->erase(ptr->begin()+i);
						
						return true;
					}
				}
				prevhash = hash;
				hash = ReHashFunction(key);
			}
			//error here: item to be deleted not found
			return false;
		}

		/**
		 * Erase an object from the hashtable without deleting it, and returns the resource
		 */
		bool Erase(const String &key, A*& res)
		{
			int32_t hash = HashFunction_1(key);

			std::vector< HashItemStr<A> > *ptr;

			int32_t prevhash = -1;
			int32_t size = 0;
			while (prevhash != hash)
			{
				ptr = &m_table[hash];
				size = (int32_t)ptr->size();
				for (int32_t i=0; i<size; ++i)
				{
					if ((*ptr)[i].GetKey() == key)
					{
						res = (*ptr)[i].GetObject();
						ptr->erase(ptr->begin()+i);
						
						return true;
					}
				}
				prevhash = hash;
				hash = ReHashFunction(key);
			}
			res = 0;
			//error here: item to be deleted not found
			return false;
		}

		/**
		 * Find a particular item in the hashtable
		 */
		HashItemStr<A> Find(const String &key)
		{
			HashItemStr<A> item;
			item.SetKey("");

			if (key == "")
				return item;

			int32_t hash = HashFunction_1(key);
			int32_t i = 0;

			std::vector<HashItemStr<A>> *ptr;

			int32_t prevhash = -1;
			int32_t size = 0;
			while (prevhash != hash)
			{
				ptr = &m_table[hash];
				size = (int32_t)ptr->size();
				for (int32_t i=0; i<size; ++i)
				{
					if ((*ptr)[i].GetKey() == key)
						return (*ptr)[i];
				}
				prevhash = hash;
				hash = ReHashFunction(key);
			}
			return item;
		}

		/**
		 * First hash function (for internal use)
		 */
		int32_t HashFunction_1(const String &str)
		{
			int32_t hash = 0;
			int32_t i,val = 0;

			for(i = 0; i < (int32_t)str.size(); ++i)
			{
				val = (int32_t)str[i];
				hash = (hash << 2) + val;
			}

			return ((hash>=0) ? (hash%m_size) : (-hash%m_size));
		}

		/**
		 * Second hash function (for internal use)
		 */
		int32_t HashFunction_2(const String &str)
		{
			int32_t hash = 0;
			int32_t i,val = 0;

			for(i = 0; i < (int32_t)str.size(); ++i)
			{
				val = (int32_t)str[i];
				hash = 1013*hash + val;
			}

			return ((hash>=0) ? (1+hash%(m_size-2)) : (-hash%m_size));
		}

		/**
		 * Function to rehash the key in the event of a collision
		 */
		int32_t ReHashFunction(const std::string &key)
		{
			int32_t hash = HashFunction_2(key);
			return (HashFunction_1(key) +  HashFunction_2(key)) % m_size;
		}

		/**
		 * Get the number of hashtable entries (both used and unused)
		 */
		int32_t GetSize()
		{
			return m_size;
		}

		//TODO:Add resize function?
	private:
		std::vector< HashItemStr<A> > *m_table;
		int32_t m_size;
};

/** @} */ // End ResourceManagement

END_ENGINE_NAMESPACE

#endif
