#include <list>
#include <vector>
#include <cstddef>

namespace golvok::allocator {

class Allocator {
	struct Chunk {
		std::byte* start;
		std::size_t size;
		bool is_free;
	};
	using ChunkList = std::list<Chunk>;
	using ChunkIter = typename ChunkList::iterator;

	struct Bin {
		std::size_t max_size;
		std::vector<ChunkIter> chunks;
	};

public:
	Allocator(std::byte* start, std::size_t size)
		: chunks{Chunk{start, size, true}}
		, bins{}
	{
		for (std::size_t bin_max_size = 1 << 5; bin_max_size != 1 << 10; bin_max_size <<= 1) {
			bins.push_back({bin_max_size, {}});
		}
		// create the biggest/fallback bin
		bins.push_back({std::numeric_limits<std::size_t>::max(), {}});

		returnChunk(chunks.begin()); // insert the initial chunk in the right place
	}

	std::byte* alloc(std::size_t size) {
		for (auto& bin : bins) {
			if (size > bin.max_size || bin.chunks.empty())
				continue;

			// find in bin's chunks and remove it
			auto big_enough_and_free = [&](auto& chunk_it) { return chunk_it->size >= size && chunk_it->is_free; };
			auto bin_chunk_it = std::find_if(bin.chunks.begin(), bin.chunks.end(), big_enough_and_free);
			if (bin_chunk_it == bin.chunks.end()) continue; // no chunks big enough? go to next bin

			auto chunk_it = *bin_chunk_it;
			bin.chunks.erase(bin_chunk_it);

			// maybe split
			if (chunk_it->size > size) {
				auto unused_part = Chunk{chunk_it->start + size, chunk_it->size - size, true};
				returnChunk(chunks.insert(std::next(chunk_it), unused_part));
				chunk_it->size = size; // resize fonud chunk
			}

			// will always use this chunk
			chunk_it->is_free = false;
			return chunk_it->start;
		}
		std::terminate();
	}

	void free(std::byte* alloced_addr) {
		// find chunk
		// opt: better data structure to avoid linear search?
		auto at_least_addr = [&](auto& chunk) { return chunk.start >= alloced_addr; };
		auto chunk_it = std::find_if(chunks.begin(), chunks.end(), at_least_addr);
		if (chunk_it->start != alloced_addr) std::terminate();
		if (chunk_it->is_free) std::terminate();

		// merge the next into us?
		if (std::next(chunk_it) != chunks.end() && std::next(chunk_it)->is_free) {
			chunk_it->size += std::next(chunk_it)->size; // we're not in a bin so no bin to update
			unBinChunk(std::next(chunk_it)); // next is now redundant
			deleteChunk(std::next(chunk_it)); // next is now redundant
		}

		// merge into prev?
		if (chunk_it != chunks.begin() && std::prev(chunk_it)->is_free) {
			const auto prev_chunk_it = std::prev(chunk_it);
			// resize prev. Must un-bin it before updating the size
			// opt: if (prev_chunk_it->size + chunk_it->size < binForSize(prev_chuck_it->size).max_size) do_nothing();
			unBinChunk(prev_chunk_it);
			prev_chunk_it->size += chunk_it->size;
			returnChunk(prev_chunk_it);
			// we're now redundant, so remove us. we weren't in a bin no no need to unbin
			deleteChunk(chunk_it);
		} else {
			// we're the good chunk if we didn't merge into prev, so return us to the pool
			chunk_it->is_free = true;
			returnChunk(chunk_it);
		}
	}

private:
	void unBinChunk(ChunkIter chunk_it) {
		auto& bin = binForSize(chunk_it->size);
		auto bin_chunk_it = std::find(bin.chunks.begin(), bin.chunks.end(), chunk_it);
		if (bin_chunk_it == bin.chunks.end()) std::terminate();
		bin.chunks.erase(bin_chunk_it);
	}
	void deleteChunk(ChunkIter chunk_it) {
		chunks.erase(chunk_it);
	}
	void returnChunk(ChunkIter chunk) {
		binForSize(chunk->size).chunks.push_back(chunk);
	}
	Bin& binForSize(std::size_t size) {
		for (auto& bin : bins) {
			if (size < bin.max_size) return bin;
		}
		std::terminate();
	}

	ChunkList chunks;
	std::vector<Bin> bins;
};

}