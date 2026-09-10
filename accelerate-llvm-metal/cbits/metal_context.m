#import <Foundation/Foundation.h>
#import <Metal/Metal.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

@interface AccMetalContext : NSObject
@property(nonatomic, strong) id<MTLDevice> device;
@property(nonatomic, strong) id<MTLCommandQueue> queue;
@end

@implementation AccMetalContext
@end

void *acc_metal_context_construct(char *error, size_t error_size) {
  @autoreleasepool {
    id<MTLDevice> device = MTLCreateSystemDefaultDevice();
    if (!device) {
      snprintf(error, error_size, "No Metal device available");
      return NULL;
    }

    id<MTLCommandQueue> queue = [device newCommandQueue];
    if (!queue) {
      snprintf(error, error_size, "Could not construct Metal command queue");
      return NULL;
    }

    AccMetalContext *context = [AccMetalContext new];
    context.device = device;
    context.queue = queue;

    return (__bridge_retained void *)context;
  }
}

void acc_metal_context_deconstruct(void *pointer) {
  @autoreleasepool {
    // transfer ownership back to ARC, releasing the context
    AccMetalContext *context = (__bridge_transfer AccMetalContext *)pointer;
    (void)context;
  }
}

void *acc_metal_pipeline_load(void *context_pointer, const char *path,
                              const char *function_name, char *error,
                              size_t error_size) {
  @autoreleasepool {
    AccMetalContext *context = (__bridge AccMetalContext *)context_pointer;

    NSString *filename = [NSString stringWithUTF8String:path];
    NSString *name = [NSString stringWithUTF8String:function_name];

    if (!context || !filename || !name) {
      snprintf(error, error_size, "Invalid context, path, or function name");
      return NULL;
    }

    NSError *failure = nil;
    id<MTLLibrary> library =
        [context.device newLibraryWithURL:[NSURL fileURLWithPath:filename]
                                    error:&failure];
    if (!library) {
      snprintf(error, error_size, "%s",
               failure.localizedDescription.UTF8String
                   ?: "Could not load Metal library");
      return NULL;
    }

    id<MTLFunction> function = [library newFunctionWithName:name];
    if (!function) {
      snprintf(error, error_size, "Metal function not found: %s",
               function_name);
      return NULL;
    }

    id<MTLComputePipelineState> pipeline =
        [context.device newComputePipelineStateWithFunction:function
                                                      error:&failure];
    if (!pipeline) {
      snprintf(error, error_size, "%s",
               failure.localizedDescription.UTF8String
                   ?: "Could not construct compute pipeline");
      return NULL;
    }

    return (__bridge_retained void *)pipeline;
  }
}

void acc_metal_pipeline_deconstruct(void *pointer) {
  @autoreleasepool {
    id<MTLComputePipelineState> pipeline =
        (__bridge_transfer id<MTLComputePipelineState>)pointer;
    (void)pipeline;
  }
}

int acc_metal_generate_i32(void *context_pointer, void *pipeline_pointer,
                           uint32_t n, void *buffer_pointer, char *error,
                           size_t error_size) {
  if (n == 0)
    return 0;

  @autoreleasepool {
    AccMetalContext *context = (__bridge AccMetalContext *)context_pointer;
    id<MTLComputePipelineState> pipeline =
        (__bridge id<MTLComputePipelineState>)pipeline_pointer;
    id<MTLBuffer> buffer = (__bridge id<MTLBuffer>)buffer_pointer;

    if (!context || !pipeline || !buffer) {
      snprintf(error, error_size, "Invalid execution argument");
      return 1;
    }

    if ((size_t)n > SIZE_MAX / sizeof(int32_t)) {
      snprintf(error, error_size, "Output size overflow");
      return 1;
    }

    size_t bytes = (size_t)n * sizeof(int32_t);
    if (bytes > buffer.length || buffer.device != context.device ||
        pipeline.device != context.device) {
      snprintf(error, error_size, "Output too small or Metal device mismatch");
      return 1;
    }

    NSUInteger width = MIN(pipeline.threadExecutionWidth,
                           pipeline.maxTotalThreadsPerThreadgroup);

    if (width == 0) {
      snprintf(error, error_size, "Invalid pipline threadgroup size");
    }

    id<MTLCommandBuffer> command = [context.queue commandBuffer];
    if (!command) {
      snprintf(error, error_size, "Could not construct command buffer");
      return 1;
    }

    id<MTLComputeCommandEncoder> encoder = [command computeCommandEncoder];
    if (!encoder) {
      snprintf(error, error_size, "Could not construct compute encoder");
      return 1;
    }

    [encoder setComputePipelineState:pipeline];
    [encoder setBuffer:buffer offset:0 atIndex:0];
    [encoder setBytes:&n length:sizeof(n) atIndex:1];

    NSUInteger groups = 1 + ((NSUInteger)n - 1) / width;
    [encoder dispatchThreadgroups:MTLSizeMake(groups, 1, 1)
            threadsPerThreadgroup:MTLSizeMake(width, 1, 1)];
    [encoder endEncoding];

    [command commit];
    [command waitUntilCompleted];

    if (command.status != MTLCommandBufferStatusCompleted) {
      snprintf(error, error_size, "%s",
               command.error.localizedDescription.UTF8String
                   ?: "Metal execution failed");
      return 1;
    }

    return 0;
  }
}

void *acc_metal_buffer_construct(void *context_pointer, size_t bytes,
                                 char *error, size_t error_size) {
  @autoreleasepool {
    AccMetalContext *context = (__bridge AccMetalContext *)context_pointer;

    if (!context || bytes == 0 || bytes > context.device.maxBufferLength) {
      snprintf(error, error_size, "Invalid Metal buffer size or context");
      return NULL;
    }

    id<MTLBuffer> buffer =
        [context.device newBufferWithLength:bytes
                                    options:MTLResourceStorageModeShared];

    if (!buffer) {
      snprintf(error, error_size, "Could not allocate Metal buffer");
    }

    return (__bridge_retained void *)buffer;
  }
}

void acc_metal_buffer_deconstruct(void *pointer) {
  @autoreleasepool {
    id<MTLBuffer> buffer = (__bridge_transfer id<MTLBuffer>)pointer;
    (void)buffer;
  }
}

int acc_metal_buffer_read(void *buffer_pointer, void *output, size_t bytes,
                          char *error, size_t error_size) {
  if (bytes == 0)
    return 0;

  @autoreleasepool {
    id<MTLBuffer> buffer = (__bridge id<MTLBuffer>)buffer_pointer;

    if (!buffer || !output || bytes > buffer.length || !buffer.contents) {
      snprintf(error, error_size, "Invalid Metal buffer read");
      return 1;
    }

    memcpy(output, buffer.contents, bytes);
    return 0;
  }
}
